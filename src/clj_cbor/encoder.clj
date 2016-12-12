(ns clj-cbor.encoder
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
      DataOutputStream)))


(defprotocol Encoder
  "Protocol for a data structure visitor which encodes CBOR."

  (encode-value*
    [encoder out x]
    "Writes the given value `x` to the `DataOutputStream`."))



;; ## Major Type Encoding

(defn- write-integer
  "Writes an integer value."
  [^DataOutputStream out n]
  (if (neg? n)
    (header/write-major-int out :negative-integer (- -1 n))
    (header/write-major-int out :unsigned-integer n)))


(defn- write-byte-string
  [^DataOutputStream out bs]
  (let [hlen (header/write-major-int out :byte-string (count bs))]
    (.write out ^bytes bs)
    (+ hlen (count bs))))


(defn- write-text-string
  [^DataOutputStream out ts]
  (let [text (.getBytes ^String ts "UTF-8")
        hlen (header/write-major-int out :text-string (count text))]
    (.write out text)
    (+ hlen (count text))))


(defn- write-boolean
  "Writes a boolean simple value to the output."
  [^DataOutputStream out x]
  (.writeByte ^DataOutputStream out (if x 0xF5 0xF4))
  1)


(defn- write-null
  "Writes a 'null' simple value to the output."
  [^DataOutputStream out]
  (.writeByte ^DataOutputStream out 0xF6)
  1)


(defn- write-undefined
  "Writes an 'undefined' simple value to the output."
  [^DataOutputStream out]
  (.writeByte ^DataOutputStream out 0xF7)
  1)


(defn- write-float
  "Writes a floating-point value to the output. Special values zero, NaN, and
  +/- Infinity are represented as 16-bit numbers, otherwise the encoding is
  determined by class."
  [^DataOutputStream out n]
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
  [^DataOutputStream out ^SimpleValue x]
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


(defn- write-array
  "Writes an array of data items to the output. The array will be encoded with
  a definite length, so `xs` will be fully realized."
  [encoder ^DataOutputStream out xs]
  (let [hlen (header/write-major-int out :data-array (count xs))]
    (reduce + hlen (map (partial encode-value* encoder out) xs))))


(defn- write-map
  "Writes a map of key/value pairs to the output. The map will be encoded with
  a definite length, so `xm` will be fully realized."
  [encoder ^DataOutputStream out xm]
  (let [hlen (header/write-major-int out :data-map (count xm))]
    (reduce-kv
      (fn encode-entry
        [sum k v]
        (let [klen (encode-value* encoder out k)
              vlen (encode-value* encoder out v)]
          (+ sum klen vlen)))
      hlen
      ; TODO: sort keys
      xm)))


(defn- write-tagged
  "Writes out a tagged value."
  ([encoder ^DataOutputStream out ^TaggedValue tv]
   (write-tagged encoder out (.tag tv) (.value tv)))
  ([encoder ^DataOutputStream out tag value]
   (let [hlen (header/write-major-int out :tagged-value tag)
         vlen (encode-value* encoder out value)]
     (+ hlen vlen))))



;; ## Encoder Implementation

(defrecord ValueEncoder
  [formatters]

  Encoder

  (encode-value*
    [this out x]
    (cond
      (nil? x) (write-null out)
      (data/boolean? x) (write-boolean out x)
      (data/bytes? x) (write-byte-string out x)
      (char? x) (write-text-string out (str x))
      (string? x) (write-text-string out x)
      ;(symbol? x) (encode-symbol this out x)
      ;(keyword? x) (encode-keyword this out x)
      (integer? x) (write-integer out x)
      (float? x) (write-float out x)
      ;(number? x) (encode-number this out x)  ; Rationals?

      ; NOTE: if want to provide handlers for records, need to branch here
      (seq? x) (write-array this out x)
      (vector? x) (write-array this out x)
      (map? x) (write-map this out x)
      ;(set? x) (encode-set this out x)  ; TODO: tagged array

      (= data/undefined x) (write-undefined out)
      (data/simple-value? x) (write-simple out x)
      (data/tagged-value? x) (write-tagged this out x)

      :else
      (if-let [formatter (formatters (class x))]
        (encode-value* this out (formatter x))
        (error/*handler*
          ::unsupported-type
          (str "No handler exists to encode objects of type: " (class x)))))))


(defn value-encoder
  ([]
   (value-encoder {}))
  ([formatters]
   (ValueEncoder. formatters)))
