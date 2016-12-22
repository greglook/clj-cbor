(ns clj-cbor.tags.numbers-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.tags.numbers :refer :all]
    [clj-cbor.test-utils :refer :all]))


(deftest bignums
  (testing "parsing checks"
    (is (thrown? IllegalArgumentException
          (parse-big-int 1 (byte-array 4))))
    (is (thrown? IllegalArgumentException
          (parse-big-int 4 (byte-array 4))))
    (is (thrown? Exception
          (parse-big-int 2 "not-bytes"))))
  (with-codec {:formatters number-formatters
               :tag-handlers number-handlers}
    (check-roundtrip 18446744073709551616N "C249010000000000000000")
    (check-roundtrip -18446744073709551617N "C349010000000000000000")))


(deftest decimal-fractions
  (testing "parsing checks"
    (is (thrown? Exception
          (parse-big-decimal 3 123)))
    (is (thrown? Exception
          (parse-big-decimal 4 "not-sequential")))
    (is (thrown? Exception
          (parse-big-decimal 4 [])))
    (is (thrown? Exception
          (parse-big-decimal 4 [0])))
    (is (thrown? Exception
          (parse-big-decimal 4 [0 123 456]))))
  (with-codec {:formatters number-formatters
               :tag-handlers number-handlers}
    (check-roundtrip 273.15M "C48221196AB3")
    (check-roundtrip 3.14159M "C482241A0004CB2F")))
