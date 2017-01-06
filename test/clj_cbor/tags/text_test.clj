(ns clj-cbor.tags.text-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.tags.text :refer :all]
    [clj-cbor.test-utils :refer :all])
  (:import
    java.net.URI
    java.util.UUID))


(deftest uri-coding
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged strings"
          (parse-uri (byte-array 4)))))
  (with-codec {:write-handlers text-write-handlers
               :read-handlers text-read-handlers}
    (check-roundtrip (URI. "http://www.example.com") "D82076687474703A2F2F7777772E6578616D706C652E636F6D")))


(deftest pattern-coding
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged strings"
          (parse-pattern (byte-array 4)))))
  (with-codec {:write-handlers text-write-handlers
               :read-handlers text-read-handlers}
    (check-roundtrip str #"abc123" "D82366616263313233")))


(deftest uuid-coding
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged byte strings"
          (parse-uuid true))))
  (with-codec {:write-handlers text-write-handlers
               :read-handlers text-read-handlers}
    (check-roundtrip (UUID/fromString "dbd559ef-333b-4f11-96b1-b0654babe844")
                     "D82550DBD559EF333B4F1196B1B0654BABE844")))
