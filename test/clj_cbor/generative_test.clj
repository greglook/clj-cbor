(ns clj-cbor.generative-test
  "Generative tests for finding complex data values which may not round-trip
  correctly."
  (:require
    [clj-cbor.core :as cbor]
    [clj-cbor.test-utils :refer [equivalent?]]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))


(defspec ^:generative round-trip-equivalence 100
  (prop/for-all [x (gen/scale #(max 20 %) gen/any-printable)]
    (equivalent? x (cbor/decode (cbor/encode x)))))
