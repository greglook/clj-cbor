(ns clj-cbor.tags.numbers-test
  (:require
    [clj-cbor.core :as cbor]
    [clj-cbor.tags.numbers :refer :all]
    [clj-cbor.test-utils :refer :all]
    [clojure.test :refer :all]))


(deftest bignums
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be represented as a tagged byte string"
          (parse-positive-bignum "not-bytes")))
    (is (thrown-with-msg? Exception #"must be represented as a tagged byte string"
          (parse-negative-bignum "not-bytes"))))
  (with-codec {:write-handlers number-write-handlers
               :read-handlers number-read-handlers}
    (check-roundtrip 18446744073709551616N "C249010000000000000000")
    (check-roundtrip -18446744073709551617N "C349010000000000000000")))


(deftest decimal-fractions
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (parse-big-decimal "not-sequential")))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (parse-big-decimal [])))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (parse-big-decimal [0])))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (parse-big-decimal [0 123 456]))))
  (with-codec {:write-handlers number-write-handlers
               :read-handlers number-read-handlers}
    (check-roundtrip 273.15M "C48221196AB3")
    (check-roundtrip 3.14159M "C482241A0004CB2F")))


(deftest rationals
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (parse-ratio "not-sequential")))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (parse-ratio [])))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (parse-ratio [0])))
    (is (thrown-with-msg? Exception #"must be represented with a two-element array"
          (parse-ratio [0 123 456]))))
  (with-codec {:write-handlers number-write-handlers
               :read-handlers number-read-handlers}
    (check-roundtrip 1/3 "D81E820103")
    (check-roundtrip 11/37 "D81E820B1825")))
