(ns clj-cbor.examples-test
  "Test examples from RFC 7049 Appendix A."
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor])
  (:import
    javax.xml.bind.DatatypeConverter))


(defn- decode-hex
  [string]
  (cbor/decode (DatatypeConverter/parseHexBinary string)))


(deftest appendix-a
  (testing "unsigned integers"
    (is (= 0 (decode-hex "00")))
    (is (= 1 (decode-hex "01")))
    (is (= 10 (decode-hex "0a")))
    (is (= 23 (decode-hex "17")))
    (is (= 24 (decode-hex "1818")))
    (is (= 25 (decode-hex "1819")))
    (is (= 100 (decode-hex "1864")))
    (is (= 1000 (decode-hex "1903e8")))
    (is (= 1000000 (decode-hex "1a000f4240")))
    (is (= 1000000000000 (decode-hex "1b000000e8d4a51000")))
    (is (= 18446744073709551615 (decode-hex "1bffffffffffffffff")))
    (is (= 18446744073709551616 (decode-hex "c249010000000000000000"))))
  (testing "negative integers"
    (is (= -18446744073709551616 (decode-hex "3bffffffffffffffff")))
    (is (= -18446744073709551617 (decode-hex "c349010000000000000000")))
    (is (= -1 (decode-hex "20")))
    (is (= -10 (decode-hex "29")))
    (is (= -100 (decode-hex "3863")))
    (is (= -1000 (decode-hex "3903e7"))))
  (testing "floating-point numbers"
    (is (= 0.0 (decode-hex "f90000")))
    (is (= -0.0 (decode-hex "f98000")))
    (is (= 1.0 (decode-hex "f93c00")))
    (is (= 1.1 (decode-hex "fb3ff199999999999a")))
    (is (= 1.5 (decode-hex "f93e00")))
    (is (= 65504.0 (decode-hex "f97bff")))
    (is (= 100000.0 (decode-hex "fa47c35000")))
    (is (= 3.4028234663852886e+38 (decode-hex "fa7f7fffff")))
    (is (= 1.0e+300 (decode-hex "fb7e37e43c8800759c")))
    (is (= 5.960464477539063e-8 (decode-hex "f90001")))
    (is (= 0.00006103515625 (decode-hex "f90400")))
    (is (= -4.0 (decode-hex "f9c400")))
    (is (= -4.1 (decode-hex "fbc010666666666666")))
    (testing "float16 special values"
      (is (= Float/POSITIVE_INFINITY (decode-hex "f97c00")))
      (is (= Float/NaN (decode-hex "f97e00")))
      (is (= Float/NEGATIVE_INFINITY (decode-hex "f9fc00"))))
    (testing "float32 special values"
      (is (= Float/POSITIVE_INFINITY (decode-hex "fa7f800000")))
      (is (= Float/NaN (decode-hex "fa7fc00000")))
      (is (= Float/NEGATIVE_INFINITY (decode-hex "faff800000"))))
    (testing "float64 special values"
      (is (= Double/POSITIVE_INFINITY (decode-hex "fb7ff0000000000000")))
      (is (= Double/NaN (decode-hex "fb7ff8000000000000")))
      (is (= Double/NEGATIVE_INFINITY (decode-hex "fbfff0000000000000")))))
  (testing "simple values"
    (is (false? (decode-hex "f4")))
    (is (true? (decode-hex "f5")))
    (is (nil? (decode-hex "f6")))
    (is (cbor/undefined? (decode-hex "f7")))
;  | simple(16)                   | 0xf0                               |
;  |                              |                                    |
;  | simple(24)                   | 0xf818                             |
;  |                              |                                    |
;  | simple(255)                  | 0xf8ff                             |
    )
  (testing "tagged values"
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
    )
  (testing "byte strings"
;  | h''                          | 0x40                               |
;  |                              |                                    |
;  | h'01020304'                  | 0x4401020304                       |
    )
  (testing "text strings"
    (is (= "" (decode-hex "60")))
    (is (= "a" (decode-hex "6161")))
    (is (= "IETF" (decode-hex "6449455446")))
    (is (= "\"\\" (decode-hex "62225c")))
    (is (= "\u00fc" (decode-hex "62c3bc")))
    (is (= "\u6c34" (decode-hex "63e6b0b4")))
    (is (= "\ud800\udd51" (decode-hex "64f0908591"))))
  (testing "arrays"
    (is (= [] (decode-hex "80")))
    (is (= [1 2 3] (decode-hex "83010203")))
    (is (= [1 [2 3] [4 5]] (decode-hex "8301820203820405")))
    (is (= [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]
           (decode-hex "98190102030405060708090a0b0c0d0e0f101112131415161718181819"))))
  (testing "maps"
    (is (= {} (decode-hex "a0")))
    (is (= {1 2, 3 4} (decode-hex "a201020304")))
    (is (= {"a" 1, "b" [2 3]} (decode-hex "a26161016162820203")))
    (is (= ["a" {"b" "c"}] (decode-hex "826161a161626163")))
    (is (= {"a" "A", "b" "B", "c" "C", "d" "D", "e" "E"}
           (decode-hex "a56161614161626142616361436164614461656145"))))
  (testing "streaming structures"
;  | (_ h'0102', h'030405')       | 0x5f42010243030405ff               |
;  |                              |                                    |
;  | (_ "strea", "ming")          | 0x7f657374726561646d696e67ff       |
;  |                              |                                    |
;  | [_ ]                         | 0x9fff                             |
;  |                              |                                    |
;  | [_ 1, [2, 3], [_ 4, 5]]      | 0x9f018202039f0405ffff             |
;  |                              |                                    |
;  | [_ 1, [2, 3], [4, 5]]        | 0x9f01820203820405ff               |
;  |                              |                                    |
;  | [1, [2, 3], [_ 4, 5]]        | 0x83018202039f0405ff               |
;  |                              |                                    |
;  | [1, [_ 2, 3], [4, 5]]        | 0x83019f0203ff820405               |
;  |                              |                                    |
;  | [_ 1, 2, 3, 4, 5, 6, 7, 8,   | 0x9f0102030405060708090a0b0c0d0e0f |
;  | 9, 10, 11, 12, 13, 14, 15,   | 101112131415161718181819ff         |
;  | 16, 17, 18, 19, 20, 21, 22,  |                                    |
;  | 23, 24, 25]                  |                                    |
;  |                              |                                    |
;  | {_ "a": 1, "b": [_ 2, 3]}    | 0xbf61610161629f0203ffff           |
;  |                              |                                    |
;  | ["a", {_ "b": "c"}]          | 0x826161bf61626163ff               |
;  |                              |                                    |
;  | {_ "Fun": true, "Amt": -2}   | 0xbf6346756ef563416d7421ff         |
    ))
