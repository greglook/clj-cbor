(ns clj-cbor.data.core-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.data.core :as data]
    [clj-cbor.test-utils :refer :all]))


(deftest byte-comparison
  (is (zero? (data/compare-bytes (byte-array []) (byte-array [])))
      "empty bytes are equal")
  (is (neg? (data/compare-bytes (byte-array []) (byte-array [0])))
      "empty bytes sort before zero byte")
  (is (pos? (data/compare-bytes (byte-array [1]) (byte-array [])))
      "one byte sorts after empty bytes")
  (is (zero? (data/compare-bytes (byte-array [0 1 2 3]) (byte-array [0 1 2 3]))))
  (is (neg? (data/compare-bytes (byte-array [0 1 2]) (byte-array [0 1 2 3]))))
  (is (pos? (data/compare-bytes (byte-array [0 1 3]) (byte-array [0 1 2]))))
  (is (neg? (data/compare-bytes (byte-array [0 0 3]) (byte-array [0 1 2])))))


(deftest simple-value-construction
  (is (data/simple-value? (data/simple-value 21)))
  (is (thrown? IllegalArgumentException
        (data/simple-value -1)))
  (is (thrown? IllegalArgumentException
        (data/simple-value 256))))
