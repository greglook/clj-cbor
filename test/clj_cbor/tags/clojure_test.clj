(ns clj-cbor.tags.clojure-test
  (:require
    [clj-cbor.tags.clojure :as tags.clj]
    [clj-cbor.test-utils :refer [check-roundtrip with-codec]]
    [clojure.test :refer [deftest testing is]]))


(deftest keywords
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged strings"
          (tags.clj/parse-symbol 123))))
  (with-codec {:write-handlers tags.clj/clojure-write-handlers
               :read-handlers tags.clj/clojure-read-handlers}
    (check-roundtrip :a "D827623A61")
    (check-roundtrip :abc/def "D827683A6162632F646566")
    (check-roundtrip 'foo "D82763666F6F")
    (check-roundtrip 'bar/baz "D827676261722F62617A")))


(deftest tagged-literals
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged two-element arrays"
          (tags.clj/parse-tagged-literal 123))))
  (with-codec {:write-handlers tags.clj/clojure-write-handlers
               :read-handlers tags.clj/clojure-read-handlers}
    (check-roundtrip (juxt :tag :form) (tagged-literal 'foo 123) "D81B8263666F6F187B")))
