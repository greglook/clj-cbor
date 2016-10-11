(ns clj-cbor.examples-test
  "Test examples from RFC 7049 Appendix A."
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.data :as data])
  (:import
    javax.xml.bind.DatatypeConverter))


(defn- decode-hex
  [string]
  (cbor/decode (DatatypeConverter/parseHexBinary string)))


(deftest unsigned-integers
  (testing "direct values"
    (is (= 0 (decode-hex "00")))
    (is (= 1 (decode-hex "01")))
    (is (= 10 (decode-hex "0a")))
    (is (= 23 (decode-hex "17"))))
  (testing "uint8"
    (is (= 24 (decode-hex "1818")))
    (is (= 25 (decode-hex "1819")))
    (is (= 100 (decode-hex "1864"))))
  (testing "uint16"
    (is (= 1000 (decode-hex "1903e8"))))
  (testing "uint32"
    (is (= 1000000 (decode-hex "1a000f4240"))))
  (testing "uint64"
    (is (= 1000000000000 (decode-hex "1b000000e8d4a51000")))
    (is (= 18446744073709551615 (decode-hex "1bffffffffffffffff")))))


(deftest negative-integers
  (testing "direct values"
    (is (= -1 (decode-hex "20")))
    (is (= -10 (decode-hex "29"))))
  (testing "int8"
    (is (= -100 (decode-hex "3863")))
    (is (= -1000 (decode-hex "3903e7"))))
  (testing "int64"
    (is (= -18446744073709551616 (decode-hex "3bffffffffffffffff")))))


(deftest floating-point-numbers
  (testing "half-precision"
    (is (instance? Float (decode-hex "f90000")))
    (is (= 0.0 (decode-hex "f90000")))
    (is (= -0.0 (decode-hex "f98000")))
    (is (= 1.0 (decode-hex "f93c00")))
    (is (= 1.5 (decode-hex "f93e00")))
    (is (= 65504.0 (decode-hex "f97bff")))
    (is (= 5.960464477539063e-8 (decode-hex "f90001")))
    (is (= 0.00006103515625 (decode-hex "f90400")))
    (is (= -4.0 (decode-hex "f9c400")))
    (is (Float/isNaN (decode-hex "f97e00")))
    (is (= Float/POSITIVE_INFINITY (decode-hex "f97c00")))
    (is (= Float/NEGATIVE_INFINITY (decode-hex "f9fc00"))))
  (testing "single-precision"
    (is (instance? Float (decode-hex "fa47c35000")))
    (is (= 100000.0 (decode-hex "fa47c35000")))
    (is (= 3.4028234663852886e+38 (decode-hex "fa7f7fffff")))
    (is (Float/isNaN (decode-hex "fa7fc00000")))
    (is (= Float/POSITIVE_INFINITY (decode-hex "fa7f800000")))
    (is (= Float/NEGATIVE_INFINITY (decode-hex "faff800000"))))
  (testing "double-precision"
    (is (instance? Double (decode-hex "fb7ff8000000000000")))
    (is (= 1.1 (decode-hex "fb3ff199999999999a")))
    (is (= 1.0e+300 (decode-hex "fb7e37e43c8800759c")))
    (is (= -4.1 (decode-hex "fbc010666666666666")))
    (is (Double/isNaN (decode-hex "fb7ff8000000000000")))
    (is (= Double/POSITIVE_INFINITY (decode-hex "fb7ff0000000000000")))
    (is (= Double/NEGATIVE_INFINITY (decode-hex "fbfff0000000000000")))))


(deftest simple-values
  (is (false? (decode-hex "f4")))
  (is (true? (decode-hex "f5")))
  (is (nil? (decode-hex "f6")))
  (is (= data/undefined (decode-hex "f7")))
  (is (= (data/simple-value 16) (decode-hex "f0")))
  (is (= (data/simple-value 24) (decode-hex "f818")))
  (is (= (data/simple-value 255) (decode-hex "f8ff"))))


#_
(deftest tagged-values
  (testing "bignum"
    (is (=  18446744073709551616 (decode-hex "c249010000000000000000")))
    (is (= -18446744073709551617 (decode-hex "c349010000000000000000"))))

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


(deftest byte-strings
  (let [bytes= (fn [expected value]
                 (and (data/bytes? value)
                      (= (seq expected) (seq value))))]
    (is (bytes= [] (decode-hex "40")))
    (is (bytes= [1 2 3 4] (decode-hex "4401020304")))
    (is (bytes= [1 2 3 4 5] (decode-hex "5f42010243030405ff")))))


(deftest text-strings
  (is (= "" (decode-hex "60")))
  (is (= "a" (decode-hex "6161")))
  (is (= "IETF" (decode-hex "6449455446")))
  (is (= "\"\\" (decode-hex "62225c")))
  (is (= "\u00fc" (decode-hex "62c3bc")))
  (is (= "\u6c34" (decode-hex "63e6b0b4")))
  (is (= "\ud800\udd51" (decode-hex "64f0908591")))
  (is (= "streaming" (decode-hex "7f657374726561646d696e67ff"))))


(deftest data-arrays
  (is (= [] (decode-hex "80")))
  (is (= [1 2 3] (decode-hex "83010203")))
  (is (= [1 [2 3] [4 5]] (decode-hex "8301820203820405")))
  (is (= [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]
         (decode-hex "98190102030405060708090a0b0c0d0e0f101112131415161718181819")))
  (testing "streaming"
    (is (= [] (decode-hex "9fff")))
    (is (= [1 [2 3] [4 5]] (decode-hex "9f018202039f0405ffff")))
    (is (= [1 [2 3] [4 5]] (decode-hex "9f01820203820405ff")))
    (is (= [1 [2 3] [4 5]] (decode-hex "83018202039f0405ff")))
    (is (= [1 [2 3] [4 5]] (decode-hex "83019f0203ff820405")))
    (is (= (range 1 26) (decode-hex "9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff")))))


(deftest data-maps
  (is (= {} (decode-hex "a0")))
  (is (= {1 2, 3 4} (decode-hex "a201020304")))
  (is (= {"a" 1, "b" [2 3]} (decode-hex "a26161016162820203")))
  (is (= ["a" {"b" "c"}] (decode-hex "826161a161626163")))
  (is (= {"a" "A", "b" "B", "c" "C", "d" "D", "e" "E"}
         (decode-hex "a56161614161626142616361436164614461656145")))
  (testing "streaming"
    (is (= {"a" 1, "b" [2 3]} (decode-hex "bf61610161629f0203ffff")))
    (is (= ["a" {"b" "c"}] (decode-hex "826161bf61626163ff")))
    (is (= {"Fun" true, "Amt" -2} (decode-hex "bf6346756ef563416d7421ff")))))
