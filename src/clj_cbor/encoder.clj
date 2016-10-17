(ns clj-cbor.encoder
  (:require
    [arrangement.core :as order]
    [clj-cbor.data :as data :refer [boolean? bytes?]]
    [clj-cbor.data.float16 :as float16]
    [clojure.string :as str])
  (:import
    (java.io
      DataOutputStream)))


;; ## Encoder Protocol

(defprotocol Encoder
  "Protocol for a data structure visitor which encodes CBOR."

  (encode-value*
    [encoder out x]
    "Writes the given value `x` to the `DataOutputStream`."))


#_
(defprotocol Encoder
  "Protocol for a data structure visitor pattern."

  (encode-nil [this out])
  (encode-boolean [this out x])
  (encode-bytes [this out x])
  (encode-string [this out x])
  (encode-character [this out x])
  (encode-symbol [this out x])
  (encode-keyword [this out x])
  (encode-number [this out x])
  (encode-seq [this out x])
  (encode-vector [this out x])
  (encode-set [this out x])
  (encode-map [this out x])
  (encode-record [this out x])
  (encode-tagged [this out x])
  (encode-unknown [this out x]))


#_
(defn- encode-value*
  "Visits values in data structures."
  [encoder out x]
  (cond
    (nil? x)     (encode-nil encoder out)
    (boolean? x) (encode-boolean encoder out x)
    (bytes? x)   (encode-bytes encoder out x)
    (char? x)    (encode-string encoder out (str x))
    (string? x)  (encode-string encoder out x)
    (symbol? x)  (encode-symbol encoder out x)
    (keyword? x) (encode-keyword encoder out x)
    (number? x)  (encode-number encoder out x)
    (seq? x)     (encode-seq encoder out x)
    (vector? x)  (encode-vector encoder out x)
    (record? x)  (encode-record encoder out x)
    (map? x)     (encode-map encoder out x)
    (set? x)     (encode-set encoder out x)
    (= data/undefined x)
      (encode-undefined encoder out x)
    (data/simple-value? x)
      (encode-simple encoder out x)
    (data/tagged-value? x)
      (encode-tagged encoder out x)
    :else
      (encode-unknown encoder out x)))



;; ## Error Handling

(defn encoder-exception!
  "Default behavior for encoding errors."
  [error-type message]
  (throw (ex-info (str "Encoding failure: " message)
                  {:error error-type})))


(def ^:dynamic *error-handler*
  "Dynamic error handler which can be bound to a function which will be called
  with a type keyword and a message."
  encoder-exception!)



;; ## Writer Functions

(let [mtype-codes (zipmap data/major-types (range))]
  (defn- write-header
    "Writes a header byte for the given major-type and additional info numbers."
    [^DataOutputStream out mtype info]
    (let [header (-> (bit-and (mtype-codes mtype) 0x07)
                     (bit-shift-left 5)
                     (bit-or (bit-and info 0x1F)))]
      (.writeByte out header))
    1))


(defn- write-int
  "Writes a header byte for the given major-type, plus extra bytes to encode
  the given integer value. Always writes the smallest possible representation."
  [^DataOutputStream out mtype i]
  (cond
    (neg? i)
      (*error-handler*
        ::negative-int-code
        (str "Cannot write negative integer code: " i))
    (<= i 23)
      (do (write-header out mtype i)
          1)
    (<= i 0xFF)
      (do (write-header out mtype 24)
          (.writeByte out i)
          2)
    (<= i 0xFFFF)
      (do (write-header out mtype 25)
          (.writeShort out i)
          3)
    (<= i Integer/MAX_VALUE)
      (do (write-header out mtype 26)
          (.writeInt out i)
          5)
    (<= i 0xFFFFFFFF)
      (do (write-header out mtype 26)
          (.writeInt out (+ Integer/MIN_VALUE (- (dec i) Integer/MAX_VALUE)))
          5)
    (<= i Long/MAX_VALUE)
      (do (write-header out mtype 27)
          (.writeLong out i)
          9)
    :else
      (do (write-header out mtype 27)
          (.writeLong out (+ Long/MIN_VALUE (- (dec i) Long/MAX_VALUE)))
          9)))



;; ## Major Type Encoding

(defn- write-positive-integer
  "Writes a positive integer value."
  [^DataOutputStream out n]
  (write-int out :unsigned-integer n))


(defn- write-negative-integer
  "Writes a negative integer value."
  [^DataOutputStream out n]
  (write-int out :negative-integer (- -1 n)))


(defn- write-byte-string
  [^DataOutputStream out bs]
  (let [hlen (write-int out :byte-string (count bs))]
    (.write out ^bytes bs)
    (+ hlen (count bs))))


(defn- write-text-string
  [^DataOutputStream out ts]
  (let [text (.getBytes ^String ts "UTF-8")
        hlen (write-int out :text-string (count text))]
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
      (do (write-header out :simple-value 25)
          (.writeShort out float16/zero)
          3)
    (Float/isNaN n)
      (do (write-header out :simple-value 25)
          (.writeShort out float16/not-a-number)
          3)
    (Float/isInfinite n)
      (do (write-header out :simple-value 25)
          (.writeShort out (if (pos? n)
                             float16/positive-infinity
                             float16/negative-infinity))
          3)
    (instance? Float n)
      (do (write-header out :simple-value 26)
          (.writeFloat out (float n))
          5)
    :else
      (do (write-header out :simple-value 27)
          (.writeDouble out (double n))
          9)))


(defn- write-simple
  "Writes a generic simple value for the given code and returns the number of
  bytes written. Does not handle floating-point or reserved values."
  [^DataOutputStream out ^long n]
  (cond
    (<= 0 n 23)
      (write-header out :simple-value n)
    (<= 32 n 255)
      (do (write-header out :simple-value 24)
          (.writeByte out n)
          2)
    :else
      (*error-handler*
        ::illegal-simple-type
        (str "Illegal or reserved simple value: " n))))


(defn- write-array
  "Writes an array of data items to the output. The array will be encoded with
  a definite length, so `xs` will be fully realized."
  [encoder ^DataOutputStream out xs]
  (let [hlen (write-int out :data-array (count xs))]
    (reduce + hlen (map (partial encode-value* encoder out) xs))))


(defn- write-map
  "Writes a map of key/value pairs to the output. The map will be encoded with
  a definite length, so `xm` will be fully realized."
  [encoder ^DataOutputStream out xm]
  (let [hlen (write-int out :data-map (count xm))]
    (reduce-kv
      (fn [sum k v]
        (let [klen (encode-value* encoder out k)
              vlen (encode-value* encoder out v)]
          (+ sum klen vlen)))
      hlen
      ; TODO: sort keys
      xm)))


(defn- write-tagged
  ""
  [encoder ^DataOutputStream out tv]
  ,,,)



;; ## Encoder Implementation

(defrecord ValueEncoder
  [handlers]

  Encoder

  (encode-value*
    [this out x]
    (cond
      (nil? x)     (write-null out)
      (boolean? x) (write-boolean out x)
      (bytes? x)   (write-byte-string out x)
      (char? x)    (write-text-string out (str x))
      (string? x)  (write-text-string out x)
      ;(symbol? x)  (encode-symbol this out x)
      ;(keyword? x) (encode-keyword this out x)
      (integer? x) (if (neg? x)
                     (write-negative-integer out x)
                     (write-positive-integer out x))
      (float? x)   (write-float out x)
      ;(number? x)  (encode-number this out x)
      (seq? x)     (write-array this out x)
      (vector? x)  (write-array this out x)
      ;(record? x)  (encode-record this out x)
      (map? x)     (write-map this out x)
      ;(set? x)     (encode-set this out x)
      (= data/undefined x) (write-undefined out)
      (data/simple-value? x) (write-simple out (.n ^clj_cbor.data.SimpleValue x))
      (data/tagged-value? x) (write-tagged this out x)
      :else ; TODO: better unknown type handling
        (write-undefined out))))
