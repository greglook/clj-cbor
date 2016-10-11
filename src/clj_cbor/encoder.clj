(ns clj-cbor.encoder
  (:require
    [arrangement.core :as order]
    [clj-cbor.data :as data :refer [boolean?]]
    [clojure.string :as str]))


(defprotocol Encoder
  "Protocol for a data structure visitor pattern."

  (encode-nil [this out])
  (encode-boolean [this out x])
  (encode-string [this out x])
  (encode-character [this out x]) ; ?
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


(defn- encode-value
  "Visits values in data structures."
  [encoder out x]
  (cond
    (nil? x)     (encode-nil encoder out)
    (boolean? x) (encode-boolean encoder out x)
    (string? x)  (encode-string encoder out x)
    (symbol? x)  (encode-symbol encoder out x)
    (keyword? x) (encode-keyword encoder out x)
    (number? x)  (encode-number encoder out x)
    (seq? x)     (encode-seq encoder out x)
    (vector? x)  (encode-vector encoder out x)
    (record? x)  (encode-record encoder out x)
    (map? x)     (encode-map encoder out x)
    (set? x)     (encode-set encoder out x)
    (tagged-literal? x) (encode-tagged encoder out x)
    :else        (encode-unknown encoder out x)))


(defrecord ValueEncoder
  [handlers]

  Encoder

  ; Primitive Types

  (encode-nil
    [this out]
    ,,,)

  ; Collection Types

  ; Special Types

  )


(defn cbor-encoder
  "Constructs a new CBOR encoder."
  []
  (map->ValueEncoder {}))
