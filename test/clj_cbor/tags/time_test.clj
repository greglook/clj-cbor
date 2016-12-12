(ns clj-cbor.tags.time-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.test-utils :refer :all])
  (:import
    java.util.Date
    java.time.Instant))


;  | 0("2013-03-21T20:04:00Z")    | 0xc074323031332d30332d32315432303a |
;  |                              | 30343a30305a                       |
;  |                              |                                    |
;  | 1(1363896240)                | 0xc11a514b67b0                     |
;  |                              |                                    |
;  | 1(1363896240.5)              | 0xc1fb41d452d9ec200000             |


#_
(deftest time-dates
  (check-roundtrip (Date. 1363896240000) "C11A514B67B0")
  (check-roundtrip (Date. 1363896240500) "C1fB41D452D9EC200000"))
