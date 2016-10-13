(ns clj-cbor.encoder-test
  "Decoding tests. Test examples are from RFC 7049 Appendix A."
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.data :as data]
    [clj-cbor.encoder :as encoder])
  (:import
    javax.xml.bind.DatatypeConverter))


(defn bytes=
  [expected value]
  (and (data/bytes? value)
       (= (seq expected) (seq value))))


(defn- encoded-hex
  [value]
  (let [buffer (java.io.ByteArrayOutputStream.)
        data-out (java.io.DataOutputStream. buffer)
        encoder (encoder/map->ValueEncoder {})]
    (encoder/encode-value* encoder data-out value)
    (DatatypeConverter/printHexBinary (.toByteArray buffer))))


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
