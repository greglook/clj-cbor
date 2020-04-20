(ns clj-cbor.tags.numbers-test
  (:require
    [clj-cbor.tags.numbers :as tags.num]
    [clj-cbor.test-utils :refer [check-roundtrip with-codec]]
    [clojure.test :refer [deftest testing is]]))


(deftest bignums
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be represented as a tagged byte string"
          (tags.num/parse-positive-bignum "not-bytes")))
    (is (thrown-with-msg? Exception #"must be represented as a tagged byte string"
          (tags.num/parse-negative-bignum "not-bytes"))))
  (with-codec {:write-handlers tags.num/number-write-handlers
               :read-handlers tags.num/number-read-handlers}
    (check-roundtrip 18446744073709551616N "C249010000000000000000")
    (check-roundtrip -18446744073709551617N "C349010000000000000000")))


(deftest decimal-fractions
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (tags.num/parse-big-decimal "not-sequential")))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (tags.num/parse-big-decimal [])))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (tags.num/parse-big-decimal [0])))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (tags.num/parse-big-decimal [0 123 456]))))
  (with-codec {:write-handlers tags.num/number-write-handlers
               :read-handlers tags.num/number-read-handlers}
    (check-roundtrip 273.15M "C48221196AB3")
    (check-roundtrip 3.14159M "C482241A0004CB2F")))


(deftest rationals
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (tags.num/parse-ratio "not-sequential")))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (tags.num/parse-ratio [])))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (tags.num/parse-ratio [0])))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (tags.num/parse-ratio [0 123 456]))))
  (with-codec {:write-handlers tags.num/number-write-handlers
               :read-handlers tags.num/number-read-handlers}
    (check-roundtrip 1/3 "D81E820103")
    (check-roundtrip 11/37 "D81E820B1825")))
