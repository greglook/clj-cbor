(ns clj-cbor.tags.text-test
  (:require
    [clj-cbor.tags.text :as tags.text]
    [clj-cbor.test-utils :refer [check-roundtrip with-codec]]
    [clojure.test :refer [deftest testing is]])
  (:import
    java.net.URI
    java.util.UUID))


(deftest uri-coding
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged strings"
          (tags.text/parse-uri (byte-array 4)))))
  (with-codec {:write-handlers tags.text/text-write-handlers
               :read-handlers tags.text/text-read-handlers}
    (check-roundtrip (URI. "http://www.example.com") "D82076687474703A2F2F7777772E6578616D706C652E636F6D")))


(deftest pattern-coding
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged strings"
          (tags.text/parse-pattern (byte-array 4)))))
  (with-codec {:write-handlers tags.text/text-write-handlers
               :read-handlers tags.text/text-read-handlers}
    (check-roundtrip str #"abc123" "D82366616263313233")))


(deftest uuid-coding
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged byte strings"
          (tags.text/parse-uuid true))))
  (with-codec {:write-handlers tags.text/text-write-handlers
               :read-handlers tags.text/text-read-handlers}
    (check-roundtrip (UUID/fromString "dbd559ef-333b-4f11-96b1-b0654babe844")
                     "D82550DBD559EF333B4F1196B1B0654BABE844")))
