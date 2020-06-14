(ns clj-cbor.tags.time-test
  (:require
    [clj-cbor.tags.time :as tags.time]
    [clj-cbor.test-utils :refer [check-roundtrip with-codec]]
    [clojure.test :refer [deftest testing is]])
  (:import
    (java.time
      Instant
      LocalDate)
    java.util.Date))


(deftest epoch-datetimes
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged numbers"
          (tags.time/parse-epoch-instant "not-a-number"))))
  (testing "java.util.Date"
    (with-codec {:write-handlers tags.time/epoch-time-write-handlers
                 :read-handlers tags.time/date-read-handlers}
      (check-roundtrip (Date. 1363896240000) "C11A514B67B0")
      (check-roundtrip (Date. 1363896240500) "C1FB41D452D9EC200000")))
  (testing "java.time.Instant"
    (with-codec {:write-handlers tags.time/epoch-time-write-handlers
                 :read-handlers tags.time/instant-read-handlers}
      (check-roundtrip (Instant/ofEpochMilli 1363896240000) "C11A514B67B0")
      (check-roundtrip (Instant/ofEpochMilli 1363896240500) "C1FB41D452D9EC200000"))))


(deftest string-datetimes
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be tagged strings"
          (tags.time/parse-string-instant 123456.789))))
  (testing "java.util.Date"
    (with-codec {:write-handlers tags.time/string-time-write-handlers
                 :read-handlers tags.time/date-read-handlers}
      (check-roundtrip (Date. 1363896240000) "C074323031332D30332D32315432303A30343A30305A")))
  (testing "java.time.Instant"
    (with-codec {:write-handlers tags.time/string-time-write-handlers
                 :read-handlers tags.time/instant-read-handlers}
      (check-roundtrip (Instant/ofEpochMilli 1363896240000) "C074323031332D30332D32315432303A30343A30305A"))))


(deftest epoch-dates
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"values must be integers"
          (tags.time/parse-epoch-local-date "not-a-number"))))
  (testing "java.time.LocalDate"
    (with-codec {:write-handlers tags.time/epoch-date-write-handlers
                 :read-handlers tags.time/local-date-read-handlers}
      (check-roundtrip (LocalDate/ofEpochDay 0) "D86400")
      (check-roundtrip (LocalDate/parse "2020-06-14") "D8641947FB"))))


(deftest string-dates
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"values must be strings"
          (tags.time/parse-string-local-date 1234))))
  (testing "java.time.LocalDate"
    (with-codec {:write-handlers tags.time/string-date-write-handlers
                 :read-handlers tags.time/local-date-read-handlers}
      (check-roundtrip (LocalDate/ofEpochDay 0) "D903EC6A313937302D30312D3031")
      (check-roundtrip (LocalDate/parse "2020-06-14") "D903EC6A323032302D30362D3134"))))
