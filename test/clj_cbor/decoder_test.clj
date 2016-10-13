(ns clj-cbor.decoder-test
  "Decoding tests. Test examples are from RFC 7049 Appendix A."
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.data :as data])
  (:import
    javax.xml.bind.DatatypeConverter))


(defn bytes=
  [expected value]
  (and (data/bytes? value)
       (= (seq expected) (seq value))))


(defn- decode-hex
  [string]
  (cbor/decode (DatatypeConverter/parseHexBinary string)))


(deftest unsigned-integers
  (testing "direct values"
    (is (= 0 (decode-hex "00")))
    (is (= 1 (decode-hex "01")))
    (is (= 10 (decode-hex "0A")))
    (is (= 23 (decode-hex "17"))))
  (testing "uint8"
    (is (= 24 (decode-hex "1818")))
    (is (= 25 (decode-hex "1819")))
    (is (= 100 (decode-hex "1864")))
    (is (= 255 (decode-hex "18FF"))))
  (testing "uint16"
    (is (= 256 (decode-hex "190100")))
    (is (= 1000 (decode-hex "1903E8")))
    (is (= 65535 (decode-hex "19FFFF"))))
  (testing "uint32"
    (is (= 65536 (decode-hex "1A00010000")))
    (is (= 1000000 (decode-hex "1A000F4240")))
    (is (= 4294967295 (decode-hex "1AFFFFFFFF"))))
  (testing "uint64"
    (is (= 4294967296 (decode-hex "1B0000000100000000")))
    (is (= 1000000000000 (decode-hex "1B000000E8D4A51000")))
    (is (= 18446744073709551615N (decode-hex "1BFFFFFFFFFFFFFFFF")))))


(deftest negative-integers
  (testing "direct values"
    (is (= -1 (decode-hex "20")))
    (is (= -10 (decode-hex "29")))
    (is (= -24 (decode-hex "37"))))
  (testing "int8"
    (is (= -25 (decode-hex "3818")))
    (is (= -100 (decode-hex "3863")))
    (is (= -256 (decode-hex "38FF"))))
  (testing "int16"
    (is (= -257 (decode-hex "390100")))
    (is (= -1000 (decode-hex "3903E7")))
    (is (= -65536 (decode-hex "39FFFF"))))
  (testing "int32"
    (is (= -65537 (decode-hex "3A00010000")))
    (is (= -1000000 (decode-hex "3A000F423F")))
    (is (= -4294967296 (decode-hex "3AFFFFFFFF"))))
  (testing "int64"
    (is (= -4294967297 (decode-hex "3B0000000100000000")))
    (is (= -18446744073709551616 (decode-hex "3BFFFFFFFFFFFFFFFF")))))


(deftest floating-point-numbers
  (testing "half-precision"
    (is (instance? Float (decode-hex "F90000")))
    (is (= 0.0 (decode-hex "F90000")))
    (is (= -0.0 (decode-hex "F98000")))
    (is (= 1.0 (decode-hex "F93C00")))
    (is (= 1.5 (decode-hex "F93E00")))
    (is (= 65504.0 (decode-hex "F97BFF")))
    (is (= 5.960464477539063e-8 (decode-hex "F90001")))
    (is (= 0.00006103515625 (decode-hex "F90400")))
    (is (= -4.0 (decode-hex "F9C400")))
    (is (Float/isNaN (decode-hex "F97E00")))
    (is (= Float/POSITIVE_INFINITY (decode-hex "F97C00")))
    (is (= Float/NEGATIVE_INFINITY (decode-hex "F9FC00"))))
  (testing "single-precision"
    (is (instance? Float (decode-hex "FA47C35000")))
    (is (= 100000.0 (decode-hex "FA47C35000")))
    (is (= 3.4028234663852886e+38 (decode-hex "FA7F7FFFFF")))
    (is (Float/isNaN (decode-hex "FA7FC00000")))
    (is (= Float/POSITIVE_INFINITY (decode-hex "FA7F800000")))
    (is (= Float/NEGATIVE_INFINITY (decode-hex "FAFF800000"))))
  (testing "double-precision"
    (is (instance? Double (decode-hex "FB7FF8000000000000")))
    (is (= 1.1 (decode-hex "FB3FF199999999999A")))
    (is (= 1.0e+300 (decode-hex "FB7E37E43C8800759C")))
    (is (= -4.1 (decode-hex "FBC010666666666666")))
    (is (Double/isNaN (decode-hex "FB7FF8000000000000")))
    (is (= Double/POSITIVE_INFINITY (decode-hex "FB7FF0000000000000")))
    (is (= Double/NEGATIVE_INFINITY (decode-hex "FBFFF0000000000000")))))


(deftest simple-values
  (is (false? (decode-hex "F4")))
  (is (true? (decode-hex "F5")))
  (is (nil? (decode-hex "F6")))
  (is (= data/undefined (decode-hex "F7")))
  (is (= (data/simple-value 16) (decode-hex "F0")))
  (is (= (data/simple-value 24) (decode-hex "F818")))
  (is (= (data/simple-value 255) (decode-hex "F8FF"))))


#_
(deftest tagged-values
  (testing "bignum"
    (is (=  18446744073709551616N (decode-hex "C249010000000000000000")))
    (is (= -18446744073709551617N (decode-hex "C349010000000000000000"))))

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
  (is (bytes= [] (decode-hex "40")))
  (is (bytes= [1 2 3 4] (decode-hex "4401020304")))
  (is (bytes= [1 2 3 4 5] (decode-hex "5F42010243030405FF"))))


(deftest text-strings
  (is (= "" (decode-hex "60")))
  (is (= "a" (decode-hex "6161")))
  (is (= "IETF" (decode-hex "6449455446")))
  (is (= "\"\\" (decode-hex "62225C")))
  (is (= "\u00fc" (decode-hex "62C3BC")))
  (is (= "\u6c34" (decode-hex "63E6B0B4")))
  (is (= "\ud800\udd51" (decode-hex "64F0908591")))
  (is (= "streaming" (decode-hex "7F657374726561646D696E67FF"))))


(deftest data-arrays
  (is (= [] (decode-hex "80")))
  (is (= [1 2 3] (decode-hex "83010203")))
  (is (= [1 [2 3] [4 5]] (decode-hex "8301820203820405")))
  (is (= [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]
         (decode-hex "98190102030405060708090A0B0C0D0E0F101112131415161718181819")))
  (testing "streaming"
    (is (= [] (decode-hex "9FFF")))
    (is (= [1 [2 3] [4 5]] (decode-hex "9F018202039F0405FFFF")))
    (is (= [1 [2 3] [4 5]] (decode-hex "9F01820203820405FF")))
    (is (= [1 [2 3] [4 5]] (decode-hex "83018202039F0405FF")))
    (is (= [1 [2 3] [4 5]] (decode-hex "83019F0203FF820405")))
    (is (= (range 1 26) (decode-hex "9F0102030405060708090A0B0C0D0E0F101112131415161718181819FF")))))


(deftest data-maps
  (is (= {} (decode-hex "A0")))
  (is (= {1 2, 3 4} (decode-hex "A201020304")))
  (is (= {"a" 1, "b" [2 3]} (decode-hex "A26161016162820203")))
  (is (= ["a" {"b" "c"}] (decode-hex "826161A161626163")))
  (is (= {"a" "A", "b" "B", "c" "C", "d" "D", "e" "E"}
         (decode-hex "A56161614161626142616361436164614461656145")))
  (testing "streaming"
    (is (= {"a" 1, "b" [2 3]} (decode-hex "BF61610161629F0203FFFF")))
    (is (= ["a" {"b" "c"}] (decode-hex "826161BF61626163FF")))
    (is (= {"Fun" true, "Amt" -2} (decode-hex "BF6346756EF563416D7421FF")))))
