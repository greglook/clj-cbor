(ns clj-cbor.tags.numbers-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.test-utils :refer :all]))


;  | 0("2013-03-21T20:04:00Z")    | 0xc074323031332d30332d32315432303a |
;  |                              | 30343a30305a                       |
;  |                              |                                    |
;  | 1(1363896240)                | 0xc11a514b67b0                     |
;  |                              |                                    |
;  | 1(1363896240.5)              | 0xc1fb41d452d9ec200000             |
;  |                              |                                    |
;  | 23(h'01020304')              | 0xd74401020304                     |
;  |                              |                                    |
;  | 24(h'6449455446')            | 0xd818456449455446                 |
;  |                              |                                    |
;  | 32("http://www.example.com") | 0xd82076687474703a2f2f7777772e6578 |
;  |                              | 616d706c652e636f6d                 |


(deftest bignums
  (testing "bignum"
    (check-roundtrip  18446744073709551616N "C249010000000000000000")
    (check-roundtrip -18446744073709551617N "C349010000000000000000")))
