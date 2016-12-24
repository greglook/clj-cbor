(ns clj-cbor.codec
  "Main CBOR codec implementation."
  (:require
    (clj-cbor
      [error :as error]
      [header :as header])
    (clj-cbor.data
      [core :as data]
      [float16 :as float16])
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
  "Protocol for a data structure visitor which encodes CBOR."

  (write-value
    [encoder out x]
    "Writes the given value `x` to the `DataOutputStream` `out`."))


(defprotocol Decoder

  (read-value*
    [decoder input header]
    "Reads a single value from the `DataInputStream`, given the just-read
    initial byte.")

  (handle-tag
    [decoder tag value]
    "Return a representation for a tagged value.")

  (unknown-simple
    [decoder value]
    "Return a representation for an unknown simple value."))


(defn read-value
  "Reads a single value from the `DataInputStream`."
  [decoder ^DataInputStream input]
  (read-value* decoder input (.readUnsignedByte input)))



;; ## Reader Functions

(def ^:private ^:const break
  "Value of the break code."
  (short 0xFF))


(defn- read-bytes
  "Reads `length` bytes from the input stream and returns them as a byte
  array."
  ^bytes
  [^DataInputStream input length]
  (let [buffer (byte-array length)]
    (.readFully input buffer)
    buffer))


(defn- read-chunks
  "Reads chunks from the input in a streaming fashion, combining them with the
  given reducing function. All chunks must have the given major type and
  definite length."
  [decoder ^DataInputStream input chunk-type reducer]
  (loop [state (reducer)]
    (let [header (.readUnsignedByte input)]
      (if (== header break)
        ; Break code, finish up result.
        (reducer state)
        ; Read next value.
        (let [[mtype info] (header/decode header)]
          (cond
            ; Illegal element type.
            (not= chunk-type mtype)
              (error/*handler*
                ::illegal-chunk
                (str chunk-type " stream may not contain chunks of type " mtype))

            ; Illegal indefinite-length chunk.
            (= info 31)
              (error/*handler*
                ::illegal-stream
                (str chunk-type " stream chunks must have a definite length"))

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



;; ## Major Types

;; ### 0, 1 - Integers

(defn- representable-integer?
  "Determines whether the given value is small enough to represent using
  the normal integer major-type."
  [value]
  (and (integer? value)
       (<= (*  2N Long/MIN_VALUE) value)
       (>  (* -2N Long/MIN_VALUE) value)))


(defn- write-integer
  "Writes an integer value."
  [encoder ^DataOutputStream out n]
  (if (neg? n)
    (header/write-major-int out :negative-integer (- -1 n))
    (header/write-major-int out :unsigned-integer n)))


(defn- read-positive-integer
  "Reads an unsigned integer from the input stream."
  [decoder ^DataInputStream input info]
  (let [value (header/read-size input info)]
    (if (= :indefinite value)
      (error/*handler*
        ::illegal-stream
        "Encoded integers cannot have indefinite length.")
      value)))


(defn- read-negative-integer
  "Reads a negative integer from the input stream."
  [decoder input info]
  (- -1 (read-positive-integer decoder input info)))



;; ### 2 - Byte Strings

(defn- write-byte-string
  [encoder ^DataOutputStream out bs]
  (let [hlen (header/write-major-int out :byte-string (count bs))]
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
  (let [length (header/read-size input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length byte strings.
      (read-chunks decoder input :byte-string concat-bytes)
      ; Read definite-length byte string.
      (read-bytes input length))))



;; ### 3 - Text Strings

(defn- write-text-string
  [encoder ^DataOutputStream out ts]
  (let [text (.getBytes ^String ts "UTF-8")
        hlen (header/write-major-int out :text-string (count text))]
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
  (let [length (header/read-size input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length text strings.
      (read-chunks decoder input :text-string concat-text)
      ; Read definite-length text string.
      (String. (read-bytes input length) "UTF-8"))))



;; ### 4 - Data Arrays

(defn- write-array
  "Writes an array of data items to the output. The array will be encoded with
  a definite length, so `xs` will be fully realized."
  [encoder ^DataOutputStream out xs]
  (let [hlen (header/write-major-int out :data-array (count xs))]
    (reduce + hlen (map (partial write-value encoder out) xs))))


(defn- build-array
  "Reducing function which builds a vector to represent a data array."
  ([] [])
  ([xs] xs)
  ([xs v] (conj xs v)))


(defn- read-array
  "Reads an array of items from the input stream."
  [decoder ^DataInputStream input info]
  (let [length (header/read-size input info)]
    (if (= length :indefinite)
      ; Read streaming sequence of elements.
      (->
        (read-value-stream decoder input build-array)
        (vary-meta assoc :cbor/streaming true))
      ; Read `length` elements.
      (->>
        (repeatedly #(read-value decoder input))
        (take length)
        (vec)))))



;; ### 5 - Data Maps

(defn- write-map
  "Writes a map of key/value pairs to the output. The map will be encoded with
  a definite length, so `xm` will be fully realized."
  [encoder ^DataOutputStream out xm]
  (let [hlen (header/write-major-int out :data-map (count xm))]
    (reduce-kv
      (fn encode-entry
        [sum k v]
        (let [klen (write-value encoder out k)
              vlen (write-value encoder out v)]
          (+ sum klen vlen)))
      hlen
      ; TODO: sort keys by encoded bytes in canonical mode
      xm)))


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
       (str "Streaming map did not contain a value for key: "
            (pr-str k)))))
  ([[m k :as state] e]
   (if (= 1 (count state))
     (if (contains? m e)
       ; Duplicate key error.
       (error/*handler*
         ::duplicate-map-key
         (str "Streaming map contains duplicate key: "
              (pr-str e)))
       ; Save key and wait for value.
       [m e])
     ; Add completed entry to map.
     [(assoc m k e)])))


(defn- read-map
  [decoder ^DataInputStream input info]
  (let [length (header/read-size input info)]
    (if (= length :indefinite)
      ; Read streaming sequence of key/value entries.
      (->
        (read-value-stream decoder input build-map)
        (vary-meta assoc :cbor/streaming true))
      ; Read `length` entry pairs.
      (->>
        (repeatedly #(read-value decoder input))
        (take (* 2 length))
        (transduce identity build-map)))))



;; ### 6 - Tagged Values

(defn- write-tagged
  "Writes out a tagged value."
  ([encoder ^DataOutputStream out ^TaggedValue tv]
   (write-tagged encoder out (.tag tv) (.value tv)))
  ([encoder ^DataOutputStream out tag value]
   (let [hlen (header/write-major-int out :tagged-value tag)
         vlen (write-value encoder out value)]
     (+ hlen vlen))))


(defn- read-tagged
  [decoder ^DataInputStream input info]
  (let [tag (header/read-size input info)
        value (read-value decoder input)]
    (try
      (handle-tag decoder tag value)
      (catch Exception ex
        (error/*handler*
          ::tag-handling-error
          (.getMessage ex)
          {:error ex})))))



;; ### 7 - Simple Values

(defn- boolean?
  "Predicate which returns true if `x` is a boolean value."
  [x]
  (or (true? x) (false? x)))


(defn- write-boolean
  "Writes a boolean simple value to the output."
  [encoder ^DataOutputStream out x]
  (.writeByte ^DataOutputStream out (if x 0xF5 0xF4))
  1)


(defn- write-null
  "Writes a 'null' simple value to the output."
  [encoder ^DataOutputStream out]
  (.writeByte ^DataOutputStream out 0xF6)
  1)


(defn- write-undefined
  "Writes an 'undefined' simple value to the output."
  [encoder ^DataOutputStream out]
  (.writeByte ^DataOutputStream out 0xF7)
  1)


(defn- write-float
  "Writes a floating-point value to the output. Special values zero, NaN, and
  +/- Infinity are represented as 16-bit numbers, otherwise the encoding is
  determined by class."
  [encoder ^DataOutputStream out n]
  (cond
    (zero? n)
      (do (header/write out :simple-value 25)
          (.writeShort out float16/zero)
          3)
    (Float/isNaN n)
      (do (header/write out :simple-value 25)
          (.writeShort out float16/not-a-number)
          3)
    (Float/isInfinite n)
      (do (header/write out :simple-value 25)
          (.writeShort out (if (pos? n)
                             float16/positive-infinity
                             float16/negative-infinity))
          3)
    (instance? Float n)
      (do (header/write out :simple-value 26)
          (.writeFloat out (float n))
          5)
    :else
      (do (header/write out :simple-value 27)
          (.writeDouble out (double n))
          9)))


(defn- write-simple
  "Writes a generic simple value for the given code and returns the number of
  bytes written. Does not handle floating-point or reserved values."
  [encoder ^DataOutputStream out ^SimpleValue x]
  (let [n (.n x)]
    (cond
      (<= 0 n 23)
        (header/write out :simple-value n)
      (<= 32 n 255)
        (do (header/write out :simple-value 24)
            (.writeByte out n)
            2)
      :else
        (error/*handler*
          ::illegal-simple-type
          (str "Illegal or reserved simple value: " n)
          {:value n}))))


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
                info))
    31 (error/*handler*
         ::unexpected-break
         "Break encountered outside streaming context.")
    (unknown-simple decoder info)))



;; ### Extension - Sets

(defn- write-set
  "Writes a set of values to the output as a tagged array."
  [encoder ^DataOutputStream out tag xs]
  ; FIXME: THIS IS NOT TO SPEC
  ; TODO: sort keys by encoded bytes in canonical mode
  (write-value encoder out (data/tagged-value tag (vec xs))))


(defn- parse-set
  [tag value]
  (when-not (sequential? value)
    (throw (ex-info (str "Sets must be tagged arrays, got: "
                         (class value))
                    {:tag tag, :value value})))
  (set value))



;; ## Codec Implementation

(defn- write-builtin
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
    (data/bytes? x) (write-byte-string codec out x)

    ; Tag extensions
    (data/tagged-value? x) (write-tagged codec out x)

    :else nil))


(defn- write-handled
  [codec out x]
  (let [dispatch (:dispatch codec)
        write-handlers (:write-handlers codec)]
    (when-let [formatter (write-handlers (dispatch x))]
      (write-value codec out (formatter x)))))


(defn- write-collection
  [codec out x]
  (cond
    (seq? x)    (write-array codec out x)
    (vector? x) (write-array codec out x)
    (map? x)    (write-map codec out x)
    (set? x)    (write-set codec out (:set-tag codec) x)))


;; - `:dispatch` function is called to provide a dispatch value based on the
;;   data to be rendered. (default: `class`)
;; - `:write-handlers` is used to look up dispatch values to find write
;;   handlers, which return an encodable representation (usually a tagged
;;   value).
;; - `:read-handlers` is used to lookup integer tags to find read handlers,
;;   which take the tag and value and return a parsed type based on the value.
;; - `:set-tag` is the integer code to tag set collections with.
(defrecord CBORCodec
  [dispatch write-handlers read-handlers set-tag]

  Encoder

  (write-value
    [this out x]
    (or (write-builtin this out x)
        (write-handled this out x)
        (write-collection this out x)
        (error/*handler*
          ::unsupported-type
          (str "No known encoding for object: " (pr-str x))
          {:value x})))


  Decoder

  (read-value*
    [this input header]
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

  (handle-tag
    [this tag value]
    (if (= tag set-tag)
      (parse-set tag value)
      (if-let [handler (read-handlers tag)]
        (handler tag value)
        ; TODO: check strict mode
        (data/tagged-value tag value))))

  (unknown-simple
    [this value]
    ; TODO: check strict mode
    (data/simple-value value)))


(def default-settings
  "Map of default properties for CBOR codecs."
  {:dispatch class
   :write-handlers {}
   :read-handlers {}
   :set-tag 13
   :canonical false
   :strict false})
