(ns clj-cbor.tags.time-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.tags.time :refer :all]
    [clj-cbor.test-utils :refer :all])
  (:import
    java.util.Date
    java.time.Instant))


(deftest epoch-datetimes
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be represented with tag 1"
          (parse-epoch-instant 8 12345)))
    (is (thrown-with-msg? Exception #"must be tagged numbers"
          (parse-epoch-instant 1 "not-a-number"))))
  (testing "java.util.Date"
    (with-codec {:formatters time-epoch-formatters
                 :tag-handlers date-handlers}
      (check-roundtrip (Date. 1363896240000) "C11A514B67B0")
      (check-roundtrip (Date. 1363896240500) "C1FB41D452D9EC200000")))
  (testing "java.time.Instant"
    (with-codec {:formatters time-epoch-formatters
                 :tag-handlers instant-handlers}
      (check-roundtrip (Instant/ofEpochMilli 1363896240000) "C11A514B67B0")
      (check-roundtrip (Instant/ofEpochMilli 1363896240500) "C1FB41D452D9EC200000"))))


(deftest string-datetimes
  (testing "parsing checks"
    (is (thrown-with-msg? Exception #"must be represented with tag 0"
          (parse-string-instant 8 "2016-12-21T19:37:42Z")))
    (is (thrown-with-msg? Exception #"must be tagged strings"
          (parse-string-instant 0 123456.789))))
  (testing "java.util.Date"
    (with-codec {:formatters time-string-formatters
                 :tag-handlers date-handlers}
      (check-roundtrip (Date. 1363896240000) "C074323031332D30332D32315432303A30343A30305A")))
  (testing "java.time.Instant"
    (with-codec {:formatters time-string-formatters
                 :tag-handlers instant-handlers}
      (check-roundtrip (Instant/ofEpochMilli 1363896240000) "C074323031332D30332D32315432303A30343A30305A"))))
