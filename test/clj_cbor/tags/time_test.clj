(ns clj-cbor.tags.time-test
  (:require
    [clj-cbor.tags.time :as tags.time]
    [clj-cbor.test-utils :refer [check-roundtrip with-codec]]
    [clojure.test :refer [deftest testing is]])
  (:import
    java.time.Instant
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
