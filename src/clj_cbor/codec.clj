(ns clj-cbor.codec
  "Main CBOR codec implementation."
  (:require
    [clj-cbor.error :as error]
    [clj-cbor.header :as header]
    [clj-cbor.data.core :as data]
    [clj-cbor.data.float16 :as float16]
    [clojure.string :as str])
  (:import
    clj_cbor.data.simple.SimpleValue
    clj_cbor.data.tagged.TaggedValue
    (java.io
      ByteArrayOutputStream
      DataInputStream
      DataOutputStream)))


;; ## Codec Protocols

(defprotocol Encoder
  "An _encoder_ is a process that generates the representation format of a CBOR
  data item from application information."

  (write-value
    [encoder out x]
    "Writes the given value `x` to the `DataOutputStream` `out`."))


(defprotocol Decoder
  "A _decoder_ is a process that reads a CBOR data item and makes it available
  to an application.

  Formally speaking, a decoder contains a parser to break up the input using
  the syntax rules of CBOR, as well as a semantic processor to prepare the data
  in a form suitable to the application."

  (read-value*
    [decoder input header]
    "Reads a single value from the `DataInputStream`, given the just-read
    initial byte."))


(defn read-value
  "Reads a single value from the `DataInputStream`."
  [decoder ^DataInputStream input]
  (read-value* decoder input (.readUnsignedByte input)))



;; ## Byte Utilities

(defn- read-bytes
  "Read `length` bytes from the input stream. Returns a byte array."
  ^bytes
  [^DataInputStream input length]
  (let [buffer (byte-array length)]
    (.readFully input buffer)
    buffer))


(defn- write-bytes
  "Writes the given value `x` to a byte array."
  [encoder x]
  (let [out (ByteArrayOutputStream.)]
    (with-open [data (DataOutputStream. out)]
      (write-value encoder data x))
    (.toByteArray out)))


(defn- compare-bytes
  "Returns a negative number, zero, or a positive number when `x` is 'less
  than', 'equal to', or 'greater than' `y`.

  Sorting is performed on the bytes of the representation of the key data
  items without paying attention to the 3/5 bit splitting for major types.
  The sorting rules are:

  - If two keys have different lengths, the shorter one sorts earlier;
  - If two keys have the same length, the one with the lower value in
    (byte-wise) lexical order sorts earlier."
  [^bytes x ^bytes y]
  (let [xlen (alength x)
        ylen (alength y)
        get-byte (fn get-byte
                   [^bytes bs i]
                   (let [b (aget bs i)]
                     (if (neg? b)
                       (+ b 256)
                       b)))]
    (if (= xlen ylen)
      ; Same length - compare content.
      (loop [i 0]
        (if (< i xlen)
          (let [xi (get-byte x i)
                yi (get-byte y i)]
            (if (= xi yi)
              (recur (inc i))
              (compare xi yi)))
          0))
      ; Compare lengths.
      (compare xlen ylen))))



;; ## Reader Functions

;; These functions provide some data-reading capabilities which later
;; major-type readers are built on. In particular, these help deal with the
;; four data types which can be _streamed_ with indefinite lengths.

(def ^:private ^:const break
  "Encoded byte representing the _break_ simple value."
  (short 0xFF))


(defn- read-chunks
  "Reads chunks from the input in a streaming fashion, combining them with the
  given reducing function. All chunks must have the given major type and
  definite length."
  [decoder ^DataInputStream input stream-type reducer]
  (loop [state (reducer)]
    (let [header (.readUnsignedByte input)]
      (if (== header break)
        ; Break code, finish up result.
        (reducer state)
        ; Read next value.
        (let [[chunk-type info] (header/decode header)]
          (cond
            ; Illegal element type.
            (not= stream-type chunk-type)
              (error/*handler*
                ::illegal-chunk-type
                (str stream-type " stream may not contain chunks of type "
                     chunk-type)
                {:stream-type stream-type
                 :chunk-type chunk-type})

            ; Illegal indefinite-length chunk.
            (= info 31)
              (error/*handler*
                ::illegal-stream
                (str stream-type " stream chunks must have a definite length")
                {:stream-type stream-type})

            ; Reduce state with next value.
            :else
              (recur (reducer state (read-value* decoder input header)))))))))


(defn- read-value-stream
  "Reads values from the input in a streaming fashion, combining them with the
  given reducing function."
  [decoder ^DataInputStream input reducer]
  (loop [state (reducer)]
    (let [header (.readUnsignedByte input)]
      (if (== header break)
        ; Break code, finish up result.
        (reducer state)
        ; Read next value.
        (recur (reducer state (read-value* decoder input header)))))))



;; ## Jump Table Decoding Utilities

;; For decoding efficiency, we can directly represent decoding operations
;; based on the first full byte of an encoded value. This can short circuit
;; conditional logic in many cases.
;;
;; These "jump entries" will be defined for some unsigned byte values
;; and be represented by a single lambda function taking the decoder
;; and input returning the decoded value
;;
;; See https://tools.ietf.org/html/rfc7049#appendix-B for details.

(defn- read-uint-expr
  "Return expression that corresponds to decoding a jump entry value. Jump
  entries either directly encode some numbers [0, 24), or specify an int-type
  (byte, short, int, long) to read from the input. Returned expression will be
  inlined into the jump entry."
  [input-sym int-type]
  (let [input-sym (vary-meta input-sym assoc :tag 'java.io.DataInputStream)]
    (cond
      (and (nat-int? int-type) (< int-type 24)) int-type
      (= int-type :byte)  `(header/read-byte ~input-sym)
      (= int-type :short) `(header/read-short ~input-sym)
      (= int-type :int)   `(header/read-int ~input-sym)
      (= int-type :long)  `(header/read-long ~input-sym))))


(defn- read-jump-entries
  "Generate jump entry code for the possible initial int values for a given
  major type. The jump values begin 24 from `offset` and read the appropriate
  bytes from the input to determine the additional info."
  [[offset input-sym val-sym] result-expr]
  (->>
    [:byte :short :int :long]
    (map-indexed
      (fn gen-jump-entry
        [idx int-type]
        [(+ offset 24 idx)
         `(let [~val-sym ~(read-uint-expr input-sym int-type)]
            ~result-expr)]))
    (apply concat)
    (vec)))


(defn- jump-entries
  "Generate jump entry code for all possible initial values for a given major
  type. The jump values begin from `offset` and read the appropriate bytes from
  the input to determine the additional info."
  [[offset input-sym val-sym]
   empty-expr
   read-expr
   stream-expr]
  (vec
    (concat
      [offset empty-expr]
      (->>
        (rest header/info-codes)
        (map-indexed
          (fn gen-jump-entry
            [idx int-type]
            [(+ offset idx 1)
             `(let [~val-sym ~(read-uint-expr input-sym int-type)]
                ~read-expr)]))
        (apply concat))
      [(+ offset 0x1F) stream-expr])))



;; ## Major Types

;; The header byte of each CBOR encoded data value uses the high-order three
;; bits to encode the _major type_ of the value. The remaining five bits
;; contain an additional information code, which often gives the size of the
;; resulting value.

;; ### Integers

;; Integers are represented by major types 0 and 1. Positive integers use type
;; 0, and the 5-bit additional information is either the integer itself (for
;; additional information values 0 through 23) or the length of additional
;; data.
;;
;; The encoding for negative integers follows the rules for unsigned integers,
;; except that the type is 1 and the value is negative one minus the encoded
;; unsigned integer.
;;
;; Additional information 24 means the value is represented in an additional
;; `uint8`, 25 means a `uint16`, 26 means a `uint32`, and 27 means a `uint64`.

(defn- representable-integer?
  "Determines whether the given value is small enough to represent using
  the normal integer major-type.

  This is made slightly trickier at the high end of the representable range by
  the JVM's lack of unsigned types, so some values that are represented in CBOR
  as 8-byte integers must be represented by `BigInt` in memory."
  [value]
  (and (integer? value)
       (<= (*  2N Long/MIN_VALUE) value)
       (>  (* -2N Long/MIN_VALUE) value)))


(defn- write-integer
  "Writes an integer value."
  [encoder ^DataOutputStream out n]
  (if (neg? n)
    (header/write out :negative-integer (-' -1 n))
    (header/write out :unsigned-integer n)))


(defn- read-positive-integer
  "Reads an unsigned integer from the input stream."
  [decoder ^DataInputStream input info]
  (let [value (header/read-code input info)]
    (if (= :indefinite value)
      (error/*handler*
        ::illegal-stream
        "Encoded integers cannot have indefinite length."
        {:code info})
      value)))


(defn- read-negative-integer
  "Reads a negative integer from the input stream."
  [decoder input info]
  (let [value (header/read-code input info)]
    (if (= :indefinite value)
      (error/*handler*
        ::illegal-stream
        "Encoded integers cannot have indefinite length."
        {:code info})
      (- -1 value))))


(defn- int-jump-entries
  "Generate jump-table entries for integer values."
  [input]
  (concat
    ; Positive integers: 0x00 - 0x1F
    (mapcat
      (fn pos-int-entry
        [idx]
        [idx idx])
      (range 24))
    (read-jump-entries
      [0x00 input 'n]
      'n)
    `[0x1F (error/*handler*
             ::illegal-stream
             "Encoded integers cannot have indefinite length."
             {:code 0x1F})]

    ; Negative integers: 0x20 - 0x2F
    (mapcat
      (fn neg-int-entry
        [idx]
        [(+ 0x20 idx) (dec (- idx))])
      (range 24))
    (read-jump-entries
      [0x20 input 'n]
      `(dec (- ~'n)))
    `[0x3F (error/*handler*
             ::illegal-stream
             "Encoded integers cannot have indefinite length."
             {:code 0x1F})]))



;; ### Byte Strings

;; Byte strings are represented by major type 2. The string's length in bytes
;; is represented following the rules for positive integers (major type 0).
;;
;; If the additional info indicates an indefinite length, the header must be
;; followed by a sequence of definite-length byte strings, terminated with a
;; break stop code. The chunks will be concatenated together into the final
;; byte string.

(defn- write-byte-string
  "Writes an array of bytes to the output string as a CBOR byte string."
  [encoder ^DataOutputStream out bs]
  (let [hlen (header/write out :byte-string (count bs))]
    (.write out ^bytes bs)
    (+ hlen (count bs))))


(defn- concat-bytes
  "Reducing function which builds a contiguous byte-array from a sequence of
  byte-array chunks."
  ([]
   (ByteArrayOutputStream.))
  ([buffer]
   (.toByteArray ^ByteArrayOutputStream buffer))
  ([buffer v]
   (.write ^ByteArrayOutputStream buffer ^bytes v)
   buffer))


(defn- read-byte-string
  "Reads a sequence of bytes from the input stream."
  [decoder ^DataInputStream input info]
  (let [length (header/read-code input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length byte strings.
      (read-chunks decoder input :byte-string concat-bytes)
      ; Read definite-length byte string.
      (read-bytes input length))))


(defn- byte-string-jump-entries
  [decoder input]
  (jump-entries
    [0x40 input 'n]
    `(byte-array 0)
    `(read-bytes ~input ~'n)
    `(read-chunks ~decoder ~input :byte-string concat-bytes)))



;; ### Text Strings

;; Major type 3 encodes a text string, specifically a string of Unicode
;; characters that is encoded as UTF-8 [RFC3629].
;;
;; The format of this type is identical to that of byte strings (major type 2),
;; that is, as with major type 2, the length gives the number of bytes. This
;; type is provided for systems that need to interpret or display
;; human-readable text, and allows the differentiation between unstructured
;; bytes and text that has a specified repertoire and encoding.
;;
;; If the additional info indicates an indefinite length, the header must be
;; followed by a sequence of definite-length text strings, terminated with a
;; break stop code. The chunks will be concatenated together into the final
;; text string.

(defn- write-text-string
  "Write a string of characters to the output as a CBOR text string."
  [encoder ^DataOutputStream out ts]
  (let [text (.getBytes ^String ts "UTF-8")
        hlen (header/write out :text-string (count text))]
    (.write out text)
    (+ hlen (count text))))


(defn- concat-text
  "Reducing function which builds a contiguous string from a sequence of string
  chunks."
  ([]
   (StringBuilder.))
  ([buffer]
   (str buffer))
  ([buffer v]
   (.append ^StringBuilder buffer ^String v)
   buffer))


(defn- read-text-string
  "Reads a sequence of bytes from the input stream."
  [decoder ^DataInputStream input info]
  (let [length (header/read-code input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length text strings.
      (read-chunks decoder input :text-string concat-text)
      ; Read definite-length text string.
      (String. (read-bytes input length) "UTF-8"))))


(defn- text-string-jump-entries
  [decoder input]
  (jump-entries
    [0x60 input 'n]
    ""
    `(String. (read-bytes ~input ~'n) "UTF-8")
    `(read-chunks ~decoder ~input :text-string concat-text)))



;; ### Data Arrays

;; Arrays of data items are encoded using major type 4. Arrays are used to
;; represent both lists and vectors in Clojure. Items in an array do not need
;; to all be of the same type.
;;
;; The array's length follows the rules for byte strings (major type 2), except
;; that the length denotes the number of data items, not the length in bytes
;; that the array takes up.
;;
;; If the additional info indicates an indefinite length, the header must be
;; followed by a sequence of element data values, terminated with a break stop
;; code.

(defn- write-array
  "Writes an array of data items to the output. The array will be encoded with
  a definite length, so `xs` will be fully realized."
  [encoder ^DataOutputStream out xs]
  (let [hlen (header/write out :data-array (count xs))]
    (reduce + hlen (map (partial write-value encoder out) xs))))


(defn- build-array
  "Reducing function which builds a vector to represent a data array."
  ([] [])
  ([xs] xs)
  ([xs v] (conj xs v)))


(defn- read-fixed-array
  "Read a fixed length array from the input as a vector of elements."
  [decoder input ^long n]
  (if (zero? n)
    []
    (loop [result (transient [])
           idx 0]
      (if (= idx n)
        (persistent! result)
        (recur (conj! result (read-value decoder input))
               (unchecked-inc idx))))))


(defn- read-array
  "Reads an array of items from the input stream."
  [decoder ^DataInputStream input info]
  (let [length (header/read-code input info)]
    (if (= length :indefinite)
      ; Read streaming sequence of elements.
      (-> (read-value-stream decoder input build-array)
          (vary-meta assoc :cbor/streaming true))
      ; Read `length` elements.
      (read-fixed-array decoder input length))))


(defn- data-array-jump-entries
  [decoder input]
  (jump-entries
    [0x80 input 'n]
    []
    `(read-fixed-array ~decoder ~input ~'n)
    `(-> (read-value-stream ~decoder ~input build-array)
         (vary-meta assoc :cbor/streaming true))))



;; ### Data Maps

;; Maps of key-value entries are encoded using major type 5. A map is comprised
;; of pairs of data items, each pair consisting of a key that is immediately
;; followed by a value.
;;
;; The map's length follows the rules for byte strings (major type 2), except
;; that the length denotes the number of pairs, not the length in bytes that
;; the map takes up.
;;
;; If the additional info indicates an indefinite length, the header must be
;; followed by a sequence of data value pairs, terminated with a break stop
;; code. An odd number of values before the break means the map is not
;; well-formed.
;;
;; A map that has duplicate keys may be well-formed, but it is not valid, and
;; thus it causes indeterminate decoding.

(defn- write-map-seq
  "Writes a sequence of key/value pairs to the output in the order given. The
  map will be encoded with a definite length, so `xm` will be fully realized."
  [encoder ^DataOutputStream out xm]
  (let [hlen (header/write out :data-map (count xm))]
    (reduce
      (fn encode-entry
        [sum [k v]]
        (let [klen (write-value encoder out k)
              vlen (write-value encoder out v)]
          (+ sum klen vlen)))
      hlen
      xm)))


(defn- write-map-canonical
  "Writes a sequence of key/value pairs to the output in canonical order. This
  requires serializing the keys in order to compare bytes."
  [encoder ^DataOutputStream out xm]
  (let [hlen (header/write out :data-map (count xm))]
    (->>
      xm
      (map (fn encode-key
             [[k v]]
             [(write-bytes encoder k) v]))
      (sort-by first compare-bytes)
      (reduce
        (fn encode-entry
          [sum [k v]]
          (.write out ^bytes k)
          (+ sum (count k) (write-value encoder out v)))
        hlen))))


(defn- write-map
  "Writes a map of key/value pairs to the output. The map will be encoded with
  a definite length, so `xm` will be fully realized."
  [encoder ^DataOutputStream out xm]
  (if (:canonical encoder)
    (write-map-canonical encoder out xm)
    (write-map-seq encoder out xm)))


(defn- build-map
  "Reducing function which builds a map from a sequence of alternating key and
  value elements."
  ([]
   [{}])
  ([[m k :as state]]
   (if (= 1 (count state))
     m
     (error/*handler*
       ::missing-map-value
       (str "Encoded map did not contain a value for key: "
            (pr-str k))
       {:map m, :key k})))
  ([[m k :as state] e]
   (if (= 1 (count state))
     (if (contains? m e)
       ; Duplicate key error.
       (error/*handler*
         ::duplicate-map-key
         (str "Encoded map contains duplicate key: "
              (pr-str e))
         {:map m, :key e})
       ; Save key and wait for value.
       [m e])
     ; Add completed entry to map.
     [(assoc m k e)])))


(defn- read-fixed-map
  "Read a fixed length map from the input as a sequence of entries."
  [decoder input ^long n]
  (if (zero? n)
    {}
    (loop [result (transient {})
           idx 0]
      (if (= idx n)
        (persistent! result)
        (let [k (read-value decoder input)]
          (if (contains? result k)
            (error/*handler*
              ::duplicate-map-key
              (str "Encoded map contains duplicate key: " (pr-str k))
              {:map (persistent! result)
               :key k})
            (recur (assoc! result k (read-value decoder input))
                   (unchecked-inc idx))))))))


(defn- read-map
  "Reads a CBOR map from the input stream, returning the constructed map
  value."
  [decoder ^DataInputStream input info]
  (let [length (header/read-code input info)]
    (if (= length :indefinite)
      ; Read streaming sequence of key/value entries.
      (-> (read-value-stream decoder input build-map)
          (vary-meta assoc :cbor/streaming true))
      ; Read `length` entry pairs.
      (read-fixed-map decoder input length))))


(defn- map-jump-entries
  [decoder input]
  (jump-entries
    [0xA0 input 'n]
    {}
    `(read-fixed-map ~decoder ~input ~'n)
    `(-> (read-value-stream ~decoder ~input build-map)
         (vary-meta assoc :cbor/streaming true))))



;; ### Sets

;; Sets are represented as arrays of elements tagged with code 258.
;;
;; This support is implemented here rather than as a normal read/write handler
;; pair for two reasons. First, unlike the normal write-handlers which operate
;; on _concrete types_, there are many types which represent the 'set' semantic
;; in Clojure, and we don't want to maintain a brittle list of such types. That
;; approach would also prevent easy extension to new set types outside the core
;; libray. Instead, we use the `set?` predicate to trigger this handler.
;;
;; Second, when the codec is in canonical mode, we want to sort the entries in
;; the set before writing them out. A write handler wouldn't have a way to know
;; whether the codec had this behavior enabled, requiring coordination between
;; the codec setting and the selection of a canonical writer vs a regular one.

(defn- write-set-seq
  "Writes a sequence of set entries to the output in the order given. The set
  will be encoded with a definite length, so `xm` will be fully realized."
  [encoder ^DataOutputStream out tag xs]
  (->>
    (vec xs)
    (data/tagged-value tag)
    (write-value encoder out)))


(defn- write-set-canonical
  "Writes a set of entries to the output in canonical order. This requires
  serializing the entries in order to compare bytes."
  [encoder ^DataOutputStream out tag xs]
  (let [tag-hlen (header/write out :tagged-value tag)
        array-hlen (header/write out :data-array (count xs))]
    (->>
      xs
      (map (partial write-bytes encoder))
      (sort compare-bytes)
      (reduce
        (fn encode-entry
          [sum v]
          (.write out ^bytes v)
          (+ sum (count v)))
        (+ tag-hlen array-hlen)))))


(defn- write-set
  "Writes a set of values to the output as a tagged array."
  [encoder ^DataOutputStream out tag xs]
  (if (:canonical encoder)
    (write-set-canonical encoder out tag xs)
    (write-set-seq encoder out tag xs)))


(defn- read-set
  "Parse a set from the value contained in the tagged representation."
  [decoder value]
  (if (sequential? value)
    (let [result (set value)]
      (if (and (:strict decoder) (< (count result) (count value)))
        (error/*handler*
          ::duplicate-set-entry
          "Encoded set contains duplicate entries"
          {:value value})
        result))
    (error/*handler*
      ::tag-handling-error
      (str "Sets must be tagged arrays, got: " (class value))
      {:value value})))



;; ### Tagged Values

;; Major type 6 is used for optional semantic tagging of other CBOR values.

(defn- write-tagged
  "Writes out a tagged value."
  ([encoder ^DataOutputStream out ^TaggedValue tv]
   (write-tagged encoder out (.tag tv) (.value tv)))
  ([encoder ^DataOutputStream out tag value]
   (let [hlen (header/write out :tagged-value tag)
         vlen (write-value encoder out value)]
     (+ hlen vlen))))


(defn- read-tagged
  [decoder ^DataInputStream input info]
  (let [tag (header/read-code input info)
        value (read-value decoder input)]
    (if (= tag (:set-tag decoder))
      (read-set decoder value)
      (try
        (if-let [handler ((:read-handlers decoder) tag)]
          ; TODO: better error reporting
          (handler value)
          (if (:strict decoder)
            (error/*handler*
              ::unknown-tag
              (str "Unknown tag code " tag)
              {:tag tag, :value value})
            (data/tagged-value tag value)))
        (catch Exception ex
          (error/*handler*
            ::tag-handling-error
            (.getMessage ex)
            (assoc (ex-data ex) ::error ex)))))))


(defn- tagged-value-jump-entries
  [decoder input info]
  [(range 0xC0 0xE0)
   `(read-tagged ~decoder ~input ~info)])



;; ### Simple Values

;; Major type 7 is for two types of data: floating-point numbers and "simple
;; values" that do not need any content, as well as the "break" stop code. Each
;; value of the 5-bit additional information in the initial byte has its own
;; separate meaning.
;;
;; Like the major types for integers, items of this major type do not carry
;; content data; all the information is in the initial bytes.

(defn- write-boolean
  "Writes a boolean simple value to the output."
  [encoder ^DataOutputStream out x]
  (.writeByte out (if x 0xF5 0xF4))
  1)


(defn- write-null
  "Writes a 'null' simple value to the output."
  [encoder ^DataOutputStream out]
  (.writeByte out 0xF6)
  1)


(defn- write-undefined
  "Writes an 'undefined' simple value to the output."
  [encoder ^DataOutputStream out]
  (.writeByte out 0xF7)
  1)


(defn- write-float
  "Writes a floating-point value to the output. Special values zero, NaN, and
  +/- Infinity are represented as 16-bit numbers, otherwise the encoding is
  determined by class."
  [encoder ^DataOutputStream out n]
  (cond
    (zero? n)
      (do (header/write-leader out :simple-value 25)
          (.writeShort out float16/zero)
          3)
    (Double/isNaN n)
      (do (header/write-leader out :simple-value 25)
          (.writeShort out float16/not-a-number)
          3)
    (Double/isInfinite n)
      (do (header/write-leader out :simple-value 25)
          (.writeShort out (if (pos? n)
                             float16/positive-infinity
                             float16/negative-infinity))
          3)
    (instance? Float n)
      (do (header/write-leader out :simple-value 26)
          (.writeFloat out (float n))
          5)
    :else
      (do (header/write-leader out :simple-value 27)
          (.writeDouble out (double n))
          9)))


(defn- write-simple
  "Writes a generic simple value for the given code and returns the number of
  bytes written. Does not handle floating-point or reserved values."
  [encoder ^DataOutputStream out ^SimpleValue x]
  (let [n (.n x)]
    (cond
      (<= 0 n 23)
        (do (header/write-leader out :simple-value n)
            1)
      (<= 32 n 255)
        (do (header/write-leader out :simple-value 24)
            (.writeByte out n)
            2)
      :else
        (error/*handler*
          ::illegal-simple-type
          (str "Illegal or reserved simple value: " n)
          {:code n}))))


(defn- unknown-simple
  [decoder value]
  (if (:strict decoder)
    (error/*handler*
      ::unknown-simple-value
      (str "Unknown simple value " value)
      {:code value})
    (data/simple-value value)))


(defn- read-simple
  "Reads a simple value from the input."
  [decoder ^DataInputStream input ^long info]
  (case info
    20 false
    21 true
    22 nil
    23 data/undefined
    24 (unknown-simple decoder (.readUnsignedByte input))
    25 (float16/decode (.readUnsignedShort input))
    26 (.readFloat input)
    27 (.readDouble input)
    (28 29 30)
      (error/*handler*
        ::illegal-simple-type
        (format "Additional information simple-value code %d is reserved."
                info)
        {:code info})
    31 (error/*handler*
         ::unexpected-break
         "Break encountered outside streaming context."
         {})
    (unknown-simple decoder info)))


(defn- simple-value-jump-entries
  [decoder input info]
  (let [input (vary-meta input assoc :tag 'java.io.DataInputStream)]
    `[~(range 0xE0 0xF4) (unknown-simple ~decoder ~info)
      0xF4 false
      0xF5 true
      0xF6 nil
      0xF7 data/undefined
      0xF8 (unknown-simple ~decoder (.readUnsignedByte ~input))
      0xF9 (float16/decode (.readUnsignedShort ~input))
      0xFA (.readFloat ~input)
      0xFB (.readDouble ~input)
      0xFF (error/*handler*
             ::unexpected-break
             "Break encountered outside streaming context."
             {})]))



;; ## Codec Implementation

(defn- write-native
  "Writes the value `x` as one of the native CBOR values and return the number
  of bytes written. Returns nil if `x` is not a native type."
  [codec out x]
  (cond
    ; Special and simple values
    (nil? x) (write-null codec out)
    (boolean? x) (write-boolean codec out x)
    (= data/undefined x) (write-undefined codec out)
    (data/simple-value? x) (write-simple codec out x)

    ; Numbers
    (representable-integer? x) (write-integer codec out x)
    (float? x) (write-float codec out x)

    ; Byte and text strings
    (char? x) (write-text-string codec out (str x))
    (string? x) (write-text-string codec out x)
    (bytes? x) (write-byte-string codec out x)

    ; Tag extensions
    (data/tagged-value? x) (write-tagged codec out x)

    :else nil))


(defn- write-handled
  "Writes the value `x` using a write-handler, if one is returned by the
  `write-handlers` lookup function. Returns the number of bytes written, or nil
  if no handler was found."
  [codec out x]
  (let [dispatch (:dispatch codec)
        write-handlers (:write-handlers codec)]
    (when-let [formatter (write-handlers (dispatch x))]
      ; TODO: better error reporting
      (write-value codec out (formatter x)))))


(defn- write-collection
  "Writes the value `x` as a collection type. Returns the number of bytes
  written, or nil if `x` is not a collection."
  [codec out x]
  (cond
    (seq? x)    (write-array codec out x)
    (vector? x) (write-array codec out x)
    (map? x)    (write-map codec out x)
    (set? x)    (write-set codec out (:set-tag codec) x)
    :else       nil))


(defmacro ^:private jump-table
  "Generate code for a case-based decoder jump table."
  [decoder input header]
  `(let [header# ~header
         info# (bit-and 0x1F header#)
         ~'info info#]
     #_
     (printf "jump-table: 0x%02X (%s:%d)\n"
             header#
             (name (first (header/decode header#)))
             info#)
     (case (int header#)
       ~@(int-jump-entries input)
       ~@(byte-string-jump-entries decoder input)
       ~@(text-string-jump-entries decoder input)
       ~@(data-array-jump-entries decoder input)
       ~@(map-jump-entries decoder input)
       ~@(tagged-value-jump-entries decoder input 'info)
       ~@(simple-value-jump-entries decoder input 'info)
       (cond
         ; Check for illegal simple types.
         (<= 0xFC header# 0xFE)
         (error/*handler*
           ::illegal-simple-type
           (format "Additional information simple-value code %d is reserved."
                   info#)
           {:code info#})

         ; Check for reserved information codes.
         (<= 28 info# 30)
         (error/*handler*
           ::header/reserved-info-code
           (format "Additional information int code %d is reserved."
                   info#)
           {:info info#})

         ; This should never happen
         :else
         (error/*handler*
           ::unreachable-case
           (format "Unhandled jump-table case: 0x%02X (%d)"
                   header# info#)
           {:header header#
            :info info#})))))


(defrecord CBORCodec
  [dispatch write-handlers read-handlers set-tag]

  Encoder

  (write-value
    [this out x]
    (or (write-native this out x)
        (write-handled this out x)
        (write-collection this out x)
        (error/*handler*
          ::unsupported-type
          (str "No known encoding for object: " (pr-str x))
          {:value x})))


  Decoder

  (read-value*
    [this input header]
    ; OPTIMIZE: try a macro-generated case statement instead
    (jump-table this input header)
    #_
    (if-let [entry-fn (and jump-table (aget jump-table header))]
      (entry-fn this input)
      (let [[mtype info] (header/decode header)]
        (case mtype
          :unsigned-integer (read-positive-integer this input info)
          :negative-integer (read-negative-integer this input info)
          :byte-string      (read-byte-string this input info)
          :text-string      (read-text-string this input info)
          :data-array       (read-array this input info)
          :data-map         (read-map this input info)
          :tagged-value     (read-tagged this input info)
          :simple-value     (read-simple this input info))))
    #_
    (let [info (bit-and 0x1F header)]
      (case (int header)
        ; Positive Integers
        0x00 0
        0x01 1
        0x02 2
        0x03 3
        0x04 4
        0x05 5
        0x06 6
        0x07 7
        0x08 8
        0x09 9
        0x0A 10
        0x0B 11
        0x0C 12
        0x0D 13
        0x0E 14
        0x0F 15
        0x10 16
        0x11 17
        0x12 18
        0x13 19
        0x14 20
        0x15 21
        0x16 22
        0x17 23
        0x18 (header/read-byte input)
        0x19 (header/read-short input)
        0x1A (header/read-int input)
        0x1B (header/read-long input)
        (0x1C 0x1D 0x1E) (error/*handler*
                           ::header/reserved-info-code
                           (format "Additional information int code %d is reserved."
                                   info)
                           {:info info})
        0x1F (error/*handler*
               ::illegal-stream
               "Encoded integers cannot have indefinite length."
               {:code info})

        ; Negative Integers
        0x20 -1
        0x21 -2
        0x22 -3
        0x23 -4
        0x24 -5
        0x25 -6
        0x26 -7
        0x27 -8
        0x28 -9
        0x29 -10
        0x2A -11
        0x2B -12
        0x2C -13
        0x2D -14
        0x2E -15
        0x2F -16
        0x30 -17
        0x31 -18
        0x32 -19
        0x33 -20
        0x34 -21
        0x35 -22
        0x36 -23
        0x37 -24
        0x38 (dec (- (header/read-byte input)))
        0x39 (dec (- (header/read-short input)))
        0x3A (dec (- (header/read-int input)))
        0x3B (dec (- (header/read-long input)))
        (0x3C 0x3D 0x3E) (error/*handler*
                           ::header/reserved-info-code
                           (format "Additional information int code %d is reserved."
                                   info)
                           {:info info})
        0x3F (error/*handler*
               ::illegal-stream
               "Encoded integers cannot have indefinite length."
               {:code info})

        ; Byte Strings
        0x40 (byte-array 0)
        0x41 (bytes/read-bytes input 1)
        0x42 (bytes/read-bytes input 2)
        0x43 (bytes/read-bytes input 3)
        0x44 (bytes/read-bytes input 4)
        0x45 (bytes/read-bytes input 5)
        0x46 (bytes/read-bytes input 6)
        0x47 (bytes/read-bytes input 7)
        0x48 (bytes/read-bytes input 8)
        0x49 (bytes/read-bytes input 9)
        0x4A (bytes/read-bytes input 10)
        0x4B (bytes/read-bytes input 11)
        0x4C (bytes/read-bytes input 12)
        0x4D (bytes/read-bytes input 13)
        0x4E (bytes/read-bytes input 14)
        0x4F (bytes/read-bytes input 15)
        0x50 (bytes/read-bytes input 16)
        0x51 (bytes/read-bytes input 17)
        0x52 (bytes/read-bytes input 18)
        0x53 (bytes/read-bytes input 19)
        0x54 (bytes/read-bytes input 20)
        0x55 (bytes/read-bytes input 21)
        0x56 (bytes/read-bytes input 22)
        0x57 (bytes/read-bytes input 23)
        0x58 (bytes/read-bytes input (header/read-byte input))
        0x59 (bytes/read-bytes input (header/read-short input))
        0x5A (bytes/read-bytes input (header/read-int input))
        0x5B (bytes/read-bytes input (header/read-long input))
        (0x5C 0x5D 0x5E) (error/*handler*
                           ::header/reserved-info-code
                           (format "Additional information int code %d is reserved."
                                   info)
                           {:info info})
        0x5F (read-chunks this input :byte-string bytes/concat-bytes)

        ; Text Strings
        0x60 ""
        0x61 (String. (bytes/read-bytes input 1) "UTF-8")
        0x62 (String. (bytes/read-bytes input 2) "UTF-8")
        0x63 (String. (bytes/read-bytes input 3) "UTF-8")
        0x64 (String. (bytes/read-bytes input 4) "UTF-8")
        0x65 (String. (bytes/read-bytes input 5) "UTF-8")
        0x66 (String. (bytes/read-bytes input 6) "UTF-8")
        0x67 (String. (bytes/read-bytes input 7) "UTF-8")
        0x68 (String. (bytes/read-bytes input 8) "UTF-8")
        0x69 (String. (bytes/read-bytes input 9) "UTF-8")
        0x6A (String. (bytes/read-bytes input 10) "UTF-8")
        0x6B (String. (bytes/read-bytes input 11) "UTF-8")
        0x6C (String. (bytes/read-bytes input 12) "UTF-8")
        0x6D (String. (bytes/read-bytes input 13) "UTF-8")
        0x6E (String. (bytes/read-bytes input 14) "UTF-8")
        0x6F (String. (bytes/read-bytes input 15) "UTF-8")
        0x70 (String. (bytes/read-bytes input 16) "UTF-8")
        0x71 (String. (bytes/read-bytes input 17) "UTF-8")
        0x72 (String. (bytes/read-bytes input 18) "UTF-8")
        0x73 (String. (bytes/read-bytes input 19) "UTF-8")
        0x74 (String. (bytes/read-bytes input 20) "UTF-8")
        0x75 (String. (bytes/read-bytes input 21) "UTF-8")
        0x76 (String. (bytes/read-bytes input 22) "UTF-8")
        0x77 (String. (bytes/read-bytes input 23) "UTF-8")
        0x78 (String. (bytes/read-bytes input (header/read-byte input)) "UTF-8")
        0x79 (String. (bytes/read-bytes input (header/read-short input)) "UTF-8")
        0x7A (String. (bytes/read-bytes input (header/read-int input)) "UTF-8")
        0x7B (String. (bytes/read-bytes input (header/read-long input)) "UTF-8")
        (0x7C 0x7D 0x7E) (error/*handler*
                           ::header/reserved-info-code
                           (format "Additional information int code %d is reserved."
                                   info)
                           {:info info})
        0x7F (read-chunks this input :text-string concat-text)

        ; Arrays
        0x80 []
        0x81 (read-fixed-array this input 1)
        0x82 (read-fixed-array this input 2)
        0x83 (read-fixed-array this input 3)
        0x84 (read-fixed-array this input 4)
        0x85 (read-fixed-array this input 5)
        0x86 (read-fixed-array this input 6)
        0x87 (read-fixed-array this input 7)
        0x88 (read-fixed-array this input 8)
        0x89 (read-fixed-array this input 9)
        0x8A (read-fixed-array this input 10)
        0x8B (read-fixed-array this input 11)
        0x8C (read-fixed-array this input 12)
        0x8D (read-fixed-array this input 13)
        0x8E (read-fixed-array this input 14)
        0x8F (read-fixed-array this input 15)
        0x90 (read-fixed-array this input 16)
        0x91 (read-fixed-array this input 17)
        0x92 (read-fixed-array this input 18)
        0x93 (read-fixed-array this input 19)
        0x94 (read-fixed-array this input 20)
        0x95 (read-fixed-array this input 21)
        0x96 (read-fixed-array this input 22)
        0x97 (read-fixed-array this input 23)
        0x98 (read-fixed-array this input (header/read-byte input))
        0x99 (read-fixed-array this input (header/read-short input))
        0x9A (read-fixed-array this input (header/read-int input))
        0x9B (read-fixed-array this input (header/read-long input))
        (0x9C 0x9D 0x9E) (error/*handler*
                           ::header/reserved-info-code
                           (format "Additional information int code %d is reserved."
                                   info)
                           {:info info})
        0x9F (-> (read-value-stream this input build-array)
                 (vary-meta assoc :cbor/streaming true))

        ; Maps
        0xA0 {}
        0xA1 (read-fixed-map this input 1)
        0xA2 (read-fixed-map this input 2)
        0xA3 (read-fixed-map this input 3)
        0xA4 (read-fixed-map this input 4)
        0xA5 (read-fixed-map this input 5)
        0xA6 (read-fixed-map this input 6)
        0xA7 (read-fixed-map this input 7)
        0xA8 (read-fixed-map this input 8)
        0xA9 (read-fixed-map this input 9)
        0xAA (read-fixed-map this input 10)
        0xAB (read-fixed-map this input 11)
        0xAC (read-fixed-map this input 12)
        0xAD (read-fixed-map this input 13)
        0xAE (read-fixed-map this input 14)
        0xAF (read-fixed-map this input 15)
        0xB0 (read-fixed-map this input 16)
        0xB1 (read-fixed-map this input 17)
        0xB2 (read-fixed-map this input 18)
        0xB3 (read-fixed-map this input 19)
        0xB4 (read-fixed-map this input 20)
        0xB5 (read-fixed-map this input 21)
        0xB6 (read-fixed-map this input 22)
        0xB7 (read-fixed-map this input 23)
        0xB8 (read-fixed-map this input (header/read-byte input))
        0xB9 (read-fixed-map this input (header/read-short input))
        0xBA (read-fixed-map this input (header/read-int input))
        0xBB (read-fixed-map this input (header/read-long input))
        (0xBC 0xBD 0xBE) (error/*handler*
                           ::header/reserved-info-code
                           (format "Additional information int code %d is reserved."
                                   info)
                           {:info info})
        0xBF (-> (read-value-stream this input build-map)
                 (vary-meta assoc :cbor/streaming true))

        ; Tagged Values
        (0xC0 0xC1 0xC2 0xC3 0xC4 0xC5 0xC6 0xC7
         0xC8 0xC9 0xCA 0xCB 0xCC 0xCD 0xCE 0xCF
         0xD0 0xD1 0xD2 0xD3 0xD4 0xD5 0xD6 0xD7
         0xD8 0xD9 0xDA 0xDB 0xDC 0xDD 0xDE 0xDF)
        (read-tagged this input info)

        ; Simple Values
        (0xE0 0xE1 0xE2 0xE3 0xE4 0xE5 0xE6 0xE7
         0xE8 0xE9 0xEA 0xEB 0xEC 0xED 0xEE 0xEF
         0xF0 0xF1 0xF2 0xF3)
        (unknown-simple this info)
        0xF4 false
        0xF5 true
        0xF6 nil
        0xF7 data/undefined
        0xF8 (unknown-simple this (.readUnsignedByte input))
        0xF9 (float16/decode (.readUnsignedShort input))
        0xFA (.readFloat input)
        0xFB (.readDouble input)
        (0xFC 0xFD 0xFE)
        (error/*handler*
          ::illegal-simple-type
          (format "Additional information simple-value code %d is reserved."
                  info)
          {:code info})
        0xFF (error/*handler*
               ::unexpected-break
               "Break encountered outside streaming context."
               {})))))


(defn blank-codec
  "Constructs a new `CBORCodec` record with default empty field values."
  []
  (map->CBORCodec
    {:dispatch class
     :write-handlers {}
     :read-handlers {}
     :set-tag 258
     :canonical false
     :strict false}))
