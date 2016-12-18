(ns clj-cbor.test-utils
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor])
  (:import
    javax.xml.bind.DatatypeConverter))


(defn bin->hex
  ^String
  [^bytes value]
  (DatatypeConverter/printHexBinary value))


(defn hex->bin
  ^bytes
  [^String value]
  (DatatypeConverter/parseHexBinary value))


(defn decode-hex
  ([string]
   (decode-hex (cbor/cbor-codec) string))
  ([decoder string]
   (first (cbor/decode decoder (hex->bin string)))))


(defn encoded-hex
  ([value]
   (encoded-hex (cbor/cbor-codec) value))
  ([encoder value]
   (bin->hex (cbor/encode encoder value))))


(defmacro check-roundtrip
  ([value hex-string]
   `(check-roundtrip cbor/default-codec ~value ~hex-string))
  ([codec value hex-string]
   `(let [~'codec ~codec]
      (is (~'= ~hex-string (encoded-hex ~'codec ~value)))
      (is (~'= ~value (decode-hex ~'codec ~hex-string))))))
