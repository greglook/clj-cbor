(ns clj-cbor.decoder-test
  "Decoding tests. Test examples are from RFC 7049 Appendix A."
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.data :as data])
  (:import
    javax.xml.bind.DatatypeConverter))


(defn bytes=
  [expected value]
  (and (data/bytes? value)
       (= (seq expected) (seq value))))


(defn- encode-hex
  [string]
  (cbor/decode (DatatypeConverter/parseHexBinary string)))


; ...
