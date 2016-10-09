(ns clj-cbor.core
  (:require
    [clj-cbor.decoder :as decoder]
    [clj-cbor.data :as data]
    [clojure.java.io :as io])
  (:import
    (clj_cbor.data
      SimpleValue
      Undefined)))


(defn decode
  [content]
  (let [input (if (string? content)
                (java.io.ByteArrayInputStream. (.getBytes ^String content))
                (io/input-stream content))]
    (decoder/decode-value input)))
