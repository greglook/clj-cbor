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
  (testing "java.util.Date"
    (let [codec (cbor/cbor-codec
                  :formatters time-epoch-formatters
                  :tag-handlers date-handlers)]
      (check-roundtrip codec = (Date. 1363896240000) "C11A514B67B0")
      (check-roundtrip codec = (Date. 1363896240500) "C1FB41D452D9EC200000")))
  (testing "java.time.Instant"
    (let [codec (cbor/cbor-codec
                  :formatters time-epoch-formatters
                  :tag-handlers instant-handlers)]
      (check-roundtrip codec = (Instant/ofEpochMilli 1363896240000) "C11A514B67B0")
      (check-roundtrip codec = (Instant/ofEpochMilli 1363896240500) "C1FB41D452D9EC200000"))))


(deftest string-datetimes
  (testing "java.util.Date"
    (let [codec (cbor/cbor-codec
                  :formatters time-string-formatters
                  :tag-handlers date-handlers)]
      (check-roundtrip codec = (Date. 1363896240000) "C074323031332D30332D32315432303A30343A30305A")))
  (testing "java.time.Instant"
    (let [codec (cbor/cbor-codec
                  :formatters time-string-formatters
                  :tag-handlers instant-handlers)]
      (check-roundtrip codec = (Instant/ofEpochMilli 1363896240000) "C074323031332D30332D32315432303A30343A30305A"))))
