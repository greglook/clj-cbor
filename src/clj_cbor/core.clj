(ns clj-cbor.core
  (:require
    [arrangement.core :as order]
    [clj-cbor.types :as ctypes]
    [clojure.string :as str]
    [multicodec.core :as codec])
  (:import
    clj_cbor.types.Undefined))


(defrecord CBORCodec
  [header write-handlers read-handlers]

  codec/Encoder

  (encodable?
    [this value]
    ,,,)


  (encode!
    [this output value]
    ,,,)


  codec/Decoder

  (decodable?
    [this header']
    (= header header'))


  (decode!
    [this input]
    ,,,))


(defn cbor-codec
  [& {:as opts}]
  (map->CBORCodec (assoc opts :header (codec/headers :cbor))))


(defn decode
  [content]
  nil)


(defn undefined?
  "Predicate which returns true if `x` is a CBOR undefined value."
  [x]
  (instance? Undefined x))
