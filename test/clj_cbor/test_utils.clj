(ns clj-cbor.test-utils
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.codec.core :as codec])
  (:import
    javax.xml.bind.DatatypeConverter))


(defn decode-hex
  ([string]
   (decode-hex (codec/map->CBORCodec {}) string))
  ([decoder string]
   (first (cbor/decode decoder (DatatypeConverter/parseHexBinary string)))))


(defn encoded-hex
  ([value]
   (encoded-hex (codec/map->CBORCodec {}) value))
  ([encoder value]
   (DatatypeConverter/printHexBinary (cbor/encode encoder value))))


(defmacro check-roundtrip
  [value hex-string]
  `(let [~'codec (codec/map->CBORCodec {})]
     (is (~'= ~hex-string (encoded-hex ~'codec ~value)))
     (is (~'= ~value (decode-hex ~'codec ~hex-string)))))
