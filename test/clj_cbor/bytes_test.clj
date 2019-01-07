(ns clj-cbor.bytes-test
  (:require [clojure.test :refer :all]
            [clj-cbor.test-utils :refer :all]
            [clj-cbor.bytes :as bytes])
  (:import
   (java.io ByteArrayInputStream
            DataInputStream)
   (java.math BigInteger)))


(defn roundtrip [bs]
  (-> bs
      ->data-input
      (bytes/read-bytes (count bs))
      seq))


(deftest read-bytes
  (are [bs]
    (= (seq bs) (roundtrip bs))
    [0 1 2]
    (range 0 128)
    []))

(deftest byte-comparison
  (is (zero? (bytes/compare-bytes (byte-array []) (byte-array [])))
      "empty bytes are equal")
  (is (neg? (bytes/compare-bytes (byte-array []) (byte-array [0])))
      "empty bytes sort before zero byte")
  (is (pos? (bytes/compare-bytes (byte-array [1]) (byte-array [])))
      "one byte sorts after empty bytes")
  (is (zero? (bytes/compare-bytes (byte-array [0 1 2 3]) (byte-array [0 1 2 3]))))
  (is (neg? (bytes/compare-bytes (byte-array [0 1 2]) (byte-array [0 1 2 3]))))
  (is (pos? (bytes/compare-bytes (byte-array [0 1 3]) (byte-array [0 1 2]))))
  (is (neg? (bytes/compare-bytes (byte-array [0 0 3]) (byte-array [0 1 2])))))


(deftest to-unsigned-long
  (are [neg-int exp]
    (= exp (bytes/to-unsigned-long neg-int))
    Long/MAX_VALUE Long/MAX_VALUE
    0 0
    -1 (BigInteger. "18446744073709551615")
    -100 (BigInteger. "18446744073709551516")
    -24  (BigInteger. "18446744073709551592")))
