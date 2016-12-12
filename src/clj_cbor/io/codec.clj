(ns clj-cbor.io.codec
  (:require
    (clj-cbor.data
      [float16 :as float16]
      [model :as data])
    (clj-cbor.io
      [error :as error]
      [header :as header])
    [clojure.string :as str])
  (:import
    (clj_cbor.data.model
      SimpleValue
      TaggedValue)
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
      (if (== header data/break)
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
                ::definite-length-required
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
      (if (== header data/break)
        ; Break code, finish up result.
        (reducer state)
        ; Read next value.
        (recur (reducer state (read-value* decoder input header)))))))



;; ## Major Types

;; ### 0, 1 - Integers

(defn- write-integer
  "Writes an integer value."
  [encoder ^DataOutputStream out n]
  (if (neg? n)
    (header/write-major-int out :negative-integer (- -1 n))
    (header/write-major-int out :unsigned-integer n)))


(defn- read-positive-integer
  "Reads an unsigned integer from the input stream."
  [decoder ^DataInputStream input info]
  (let [value (header/read-int input info)]
    (if (= :indefinite value)
      (error/*handler*
        ::definite-length-required
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
  (let [length (header/read-int input info)]
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
  (let [length (header/read-int input info)]
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
  (let [length (header/read-int input info)]
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
      ; TODO: sort keys
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
  (let [length (header/read-int input info)]
    (if (= length :indefinite)
      ; Read streaming sequence of key/value entries.
      (read-value-stream decoder input build-map)
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
  (let [tag (header/read-int input info)
        value (read-value decoder input)]
    (handle-tag decoder tag value)))



;; ### 7 - Simple Values

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
    25 (float16/from-bits (.readUnsignedShort input))
    26 (.readFloat input)
    27 (.readDouble input)
    (28 29 30)
      (error/*handler*
        ::reserved-simple-type
        (format "Additional information simple-value code %d is reserved."
                info))
    31 (error/*handler*
         ::unexpected-break
         "Break encountered outside streaming context.")
    (unknown-simple decoder info)))



;; ## Codec Types

(defrecord CBORCodec
  [formatters tag-handlers]

  Encoder

  (write-value
    [this out x]
    (cond
      (nil? x) (write-null this out)
      (data/boolean? x) (write-boolean this out x)
      (data/bytes? x) (write-byte-string this out x)
      (char? x) (write-text-string this out (str x))
      (string? x) (write-text-string this out x)
      ;(symbol? x) (encode-symbol this out x)
      ;(keyword? x) (encode-keyword this out x)
      (integer? x) (write-integer this out x)
      (float? x) (write-float this out x)
      ;(number? x) (encode-number this out x)  ; Rationals?

      ; NOTE: if want to provide handlers for records, need to branch here
      (seq? x) (write-array this out x)
      (vector? x) (write-array this out x)
      (map? x) (write-map this out x)
      ;(set? x) (encode-set this out x)  ; TODO: tagged array

      (= data/undefined x) (write-undefined this out)
      (data/simple-value? x) (write-simple this out x)
      (data/tagged-value? x) (write-tagged this out x)

      :else
      (if-let [formatter (formatters (class x))]
        (write-value this out (formatter x))
        (error/*handler*
          ::unsupported-type
          (str "No handler exists to encode objects of type: " (class x))))))


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
    (if-let [handler (get tag-handlers tag)]
      (handler tag value)
      (data/tagged-value tag value)))

  (unknown-simple
    [this value]
    (data/simple-value value)))
