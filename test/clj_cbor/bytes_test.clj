(ns clj-cbor.bytes-test
  (:require
    [clj-cbor.bytes :as bytes]
    [clj-cbor.test-utils :refer :all]
    [clojure.test :refer :all])
  (:import
    (java.io
      ByteArrayInputStream
      DataInputStream)
    java.math.BigInteger))


(defn roundtrip
  [bs]
  (-> (->data-input bs)
      (bytes/read-bytes (count bs))
      (seq)))


(deftest read-bytes
  (are [bs] (= (seq bs) (roundtrip bs))
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
  (is (neg? (bytes/compare-bytes (byte-array [0 0 3]) (byte-array [0 1 2]))))
  (is (neg? (bytes/compare-bytes (byte-array [0 0 3]) (byte-array [0 -8 2])))))
