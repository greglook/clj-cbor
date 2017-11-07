(ns clj-cbor.generative-test
  "Decoding tests. Test examples are from RFC 7049 Appendix A."
  (:require
    [clojure.test :refer :all]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clj-cbor.core :as cbor]
    [clj-cbor.data.core :as data]
    [clj-cbor.test-utils :refer :all]))


(defspec ^:generative round-trip-equivalence 100
  (prop/for-all [x gen/any-printable]
    (equivalent x (cbor/decode (cbor/encode x)))))
