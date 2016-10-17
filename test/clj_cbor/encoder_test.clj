(ns clj-cbor.encoder-test
  "Decoding tests. Test examples are from RFC 7049 Appendix A."
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.data :as data]
    [clj-cbor.encoder :as encoder])
  (:import
    javax.xml.bind.DatatypeConverter))


(defn- encoded-hex
  [value]
  (DatatypeConverter/printHexBinary (cbor/encode value)))


(deftest unsigned-integers
  (testing "direct values"
    (is (= "00" (encoded-hex 0)))
    (is (= "01" (encoded-hex 1)))
    (is (= "0A" (encoded-hex 10)))
    (is (= "17" (encoded-hex 23))))
  (testing "uint8"
    (is (= "1818" (encoded-hex 24)))
    (is (= "1819" (encoded-hex 25)))
    (is (= "1864" (encoded-hex 100)))
    (is (= "18FF" (encoded-hex 255))))
  (testing "uint16"
    (is (= "190100" (encoded-hex 256)))
    (is (= "1903E8" (encoded-hex 1000)))
    (is (= "19FFFF" (encoded-hex 65535))))
  (testing "uint32"
    (is (= "1A00010000" (encoded-hex 65536)))
    (is (= "1A000F4240" (encoded-hex 1000000)))
    (is (= "1AFFFFFFFF" (encoded-hex 4294967295))))
  (testing "uint64"
    (is (= "1B0000000100000000" (encoded-hex 4294967296)))
    (is (= "1B000000E8D4A51000" (encoded-hex 1000000000000)))
    (is (= "1BFFFFFFFFFFFFFFFF" (encoded-hex 18446744073709551615N)))))


(deftest negative-integers
  (testing "direct values"
    (is (= "20" (encoded-hex -1)))
    (is (= "29" (encoded-hex -10)))
    (is (= "37" (encoded-hex -24))))
  (testing "int8"
    (is (= "3818" (encoded-hex -25)))
    (is (= "3863" (encoded-hex -100)))
    (is (= "38FF" (encoded-hex -256))))
  (testing "int16"
    (is (= "390100" (encoded-hex -257)))
    (is (= "3903E7" (encoded-hex -1000)))
    (is (= "39FFFF" (encoded-hex -65536))))
  (testing "int32"
    (is (= "3A00010000" (encoded-hex -65537)))
    (is (= "3A000F423F" (encoded-hex -1000000)))
    (is (= "3AFFFFFFFF" (encoded-hex -4294967296))))
  (testing "int64"
    (is (= "3B0000000100000000" (encoded-hex -4294967297)))
    (is (= "3BFFFFFFFFFFFFFFFF" (encoded-hex -18446744073709551616)))))


(deftest floating-point-numbers
  (testing "special values"
    (is (= "F90000" (encoded-hex 0.0)))
    (is (= "F90000" (encoded-hex -0.0)))
    (is (= "F97E00" (encoded-hex Float/NaN)))
    (is (= "F97C00" (encoded-hex Float/POSITIVE_INFINITY)))
    (is (= "F9FC00" (encoded-hex Float/NEGATIVE_INFINITY))))
  (testing "single-precision"
    (is (= "FA47C35000" (encoded-hex (float 100000.0))))
    (is (= "FA7F7FFFFF" (encoded-hex (float 3.4028234663852886e+38)))))
  (testing "double-precision"
    (is (= "FB3FF199999999999A" (encoded-hex 1.1)))
    #_(is (= "FB7E37E43C8800759C" (encoded-hex 1.0e+300)))
    (is (= "FBC010666666666666" (encoded-hex -4.1)))))


(deftest simple-values
  (is (= "F4" (encoded-hex false)))
  (is (= "F5" (encoded-hex true)))
  (is (= "F6" (encoded-hex nil)))
  (is (= "F7" (encoded-hex data/undefined)))
  (is (= "F0" (encoded-hex (data/simple-value 16))))
  (is (= "F820" (encoded-hex (data/simple-value 32))))
  (is (= "F8FF" (encoded-hex (data/simple-value 255)))))


; TODO: tagged-values


(deftest byte-strings
  (is (= "40" (encoded-hex (byte-array 0))))
  (is (= "4401020304" (encoded-hex (byte-array [1 2 3 4])))))


(deftest text-strings
  (is (= "60" (encoded-hex "")))
  (is (= "6161" (encoded-hex "a")))
  (is (= "6449455446" (encoded-hex "IETF")))
  (is (= "62225C" (encoded-hex "\"\\")))
  (is (= "62C3BC" (encoded-hex "\u00fc")))
  (is (= "63E6B0B4" (encoded-hex "\u6c34")))
  (is (= "64F0908591" (encoded-hex "\ud800\udd51"))))


(deftest data-arrays
  (is (= "80" (encoded-hex [])))
  (is (= "83010203" (encoded-hex [1 2 3])))
  (is (= "8301820203820405" (encoded-hex [1 [2 3] [4 5]])))
  (is (= "98190102030405060708090A0B0C0D0E0F101112131415161718181819"
         (encoded-hex (range 1 26)))))


(deftest data-maps
  (is (= "A0" (encoded-hex {})))
  (is (= "A201020304" (encoded-hex {1 2, 3 4})))
  (is (= "A26161016162820203" (encoded-hex {"a" 1, "b" [2 3]})))
  (is (= "826161A161626163" (encoded-hex ["a" {"b" "c"}])))
  (is (= "A56161614161626142616361436164614461656145"
         (encoded-hex {"a" "A", "b" "B", "c" "C", "d" "D", "e" "E"}))))
