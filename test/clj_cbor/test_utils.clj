(ns clj-cbor.test-utils
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor])
  (:import
    javax.xml.bind.DatatypeConverter))


;; ## Hex Conversion Functions

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
   (first (doall (cbor/decode decoder (hex->bin string))))))


(defn encoded-hex
  ([value]
   (encoded-hex (cbor/cbor-codec) value))
  ([encoder value]
   (bin->hex (cbor/encode encoder value))))



;; ## Dynamic Codec

(def ^:dynamic *test-codec*
  (cbor/cbor-codec))


(defmacro with-codec
  [opts & body]
  `(binding [*test-codec* (cbor/cbor-codec ~@(flatten (seq opts)))]
     ~@body))


(defmacro check-roundtrip
  ([value hex-string]
   `(do (is (~'= ~hex-string (encoded-hex *test-codec* ~value)))
        (is (~'= ~value (decode-hex *test-codec* ~hex-string)))))
  ([compare-by value hex-string]
   `(do (is (~'= ~hex-string (encoded-hex *test-codec* ~value)))
        (is (~'= (~compare-by ~value) (~compare-by (decode-hex *test-codec* ~hex-string)))))))
