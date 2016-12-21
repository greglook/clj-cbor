(ns clj-cbor.tags.numbers-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.tags.numbers :refer :all]
    [clj-cbor.test-utils :refer :all]))


(deftest bignums
  (let [codec (cbor/cbor-codec
                :formatters number-formatters
                :tag-handlers number-handlers)]
    (check-roundtrip codec =  18446744073709551616N "C249010000000000000000")
    (check-roundtrip codec = -18446744073709551617N "C349010000000000000000")))


(deftest decimal-fractions
  (let [codec (cbor/cbor-codec
                :formatters number-formatters
                :tag-handlers number-handlers)]
    (check-roundtrip codec =  273.15M "C48221196AB3")
    (check-roundtrip codec = 3.14159M "C482241A0004CB2F")))
