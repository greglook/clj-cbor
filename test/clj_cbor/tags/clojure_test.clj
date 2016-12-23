(ns clj-cbor.tags.clojure-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.tags.clojure :refer :all]
    [clj-cbor.test-utils :refer :all]))


(deftest keywords
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged strings"
          (parse-symbol 39 123))))
  (with-codec {:write-handlers clojure-write-handlers
               :read-handlers clojure-read-handlers}
    (check-roundtrip :a "D827623A61")
    (check-roundtrip :abc/def "D827683A6162632F646566")
    (check-roundtrip 'foo "D82763666F6F")
    (check-roundtrip 'bar/baz "D827676261722F62617A")))


(deftest tagged-literals
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged two-element arrays"
          (parse-tagged-literal 27 123))))
  (with-codec {:write-handlers clojure-write-handlers
               :read-handlers clojure-read-handlers}
    (check-roundtrip (juxt :tag :form) (tagged-literal 'foo 123) "D81B8263666F6F187B")))
