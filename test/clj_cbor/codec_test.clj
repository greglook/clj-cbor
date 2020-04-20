(ns clj-cbor.codec-test
  "Decoding tests. Test examples are from RFC 7049 Appendix A."
  (:require
    [clj-cbor.codec :as codec]
    [clj-cbor.core :as cbor]
    [clj-cbor.data.core :as data]
    [clj-cbor.test-utils
     :refer [bytes= check-roundtrip decode-hex encoded-hex with-codec]]
    [clojure.test :refer [deftest testing is are]]))


(deftest byte-utils
  (testing "byte comparison"
    (is (zero? (#'codec/compare-bytes (byte-array []) (byte-array [])))
        "empty bytes are equal")
    (is (neg? (#'codec/compare-bytes (byte-array []) (byte-array [0])))
        "empty bytes sort before zero byte")
    (is (pos? (#'codec/compare-bytes (byte-array [1]) (byte-array [])))
        "one byte sorts after empty bytes")
    (is (zero? (#'codec/compare-bytes (byte-array [0 1 2 3]) (byte-array [0 1 2 3]))))
    (is (neg? (#'codec/compare-bytes (byte-array [0 1 2]) (byte-array [0 1 2 3]))))
    (is (pos? (#'codec/compare-bytes (byte-array [0 1 3]) (byte-array [0 1 2]))))
    (is (neg? (#'codec/compare-bytes (byte-array [0 0 3]) (byte-array [0 1 2]))))
    (is (neg? (#'codec/compare-bytes (byte-array [0 0 3]) (byte-array [0 -8 2]))))))


(deftest integer-typing
  (let [roundtrip (comp cbor/decode cbor/encode)]
    (testing "direct values"
      (are [i] (instance? Long (roundtrip i))
        -24 -1 0 1 23))
    (testing "int8"
      (are [i] (instance? Long (roundtrip i))
        -256 -25 24 255))
    (testing "int16"
      (are [i] (instance? Long (roundtrip i))
        -65536 -257 256 65535))
    (testing "int32"
      (are [i] (instance? Long (roundtrip i))
        -4294967296 -65537 65536 4294967295))
    (testing "int64"
      (are [i] (instance? Long (roundtrip i))
        Long/MIN_VALUE -4294967297 4294967296 Long/MAX_VALUE))
    (testing "int64+"
      (are [i] (instance? clojure.lang.BigInt (roundtrip i))
        -18446744073709551616N
        (dec' Long/MIN_VALUE)
        (inc' Long/MAX_VALUE)
        18446744073709551615N))))


(deftest unsigned-integers
  (testing "direct values"
    (check-roundtrip 0 "00")
    (check-roundtrip 1 "01")
    (check-roundtrip 10 "0A")
    (check-roundtrip 23 "17"))
  (testing "uint8"
    (check-roundtrip 24 "1818")
    (check-roundtrip 25 "1819")
    (check-roundtrip 100 "1864")
    (check-roundtrip 255 "18FF"))
  (testing "uint16"
    (check-roundtrip 256 "190100")
    (check-roundtrip 1000 "1903E8")
    (check-roundtrip 65535 "19FFFF"))
  (testing "uint32"
    (check-roundtrip 65536 "1A00010000")
    (check-roundtrip 1000000 "1A000F4240")
    (check-roundtrip 4294967295 "1AFFFFFFFF"))
  (testing "uint64"
    (check-roundtrip 4294967296 "1B0000000100000000")
    (check-roundtrip 1000000000000 "1B000000E8D4A51000")
    (check-roundtrip Long/MAX_VALUE "1B7FFFFFFFFFFFFFFF")
    (check-roundtrip 18446744073709551615N "1BFFFFFFFFFFFFFFFF"))
  (testing "errors"
    (is (cbor-error? ::codec/illegal-stream
          (decode-hex "1F00")))))


(deftest negative-integers
  (testing "direct values"
    (check-roundtrip -1 "20")
    (check-roundtrip -10 "29")
    (check-roundtrip -24 "37"))
  (testing "int8"
    (check-roundtrip -25 "3818")
    (check-roundtrip -100 "3863")
    (check-roundtrip -256 "38FF"))
  (testing "int16"
    (check-roundtrip -257 "390100")
    (check-roundtrip -1000 "3903E7")
    (check-roundtrip -65536 "39FFFF"))
  (testing "int32"
    (check-roundtrip -65537 "3A00010000")
    (check-roundtrip -1000000 "3A000F423F")
    (check-roundtrip -4294967296 "3AFFFFFFFF"))
  (testing "int64"
    (check-roundtrip -4294967297 "3B0000000100000000")
    (check-roundtrip Long/MIN_VALUE "3B7FFFFFFFFFFFFFFF")
    (check-roundtrip -18446744073709551616 "3BFFFFFFFFFFFFFFFF"))
  (testing "errors"
    (is (cbor-error? ::codec/illegal-stream
          (decode-hex "3F00")))))


(deftest byte-strings
  (testing "direct bytes"
    (is (= "40" (encoded-hex (byte-array 0))))
    (is (bytes= [] (decode-hex "40")))
    (is (= "4401020304" (encoded-hex (byte-array [1 2 3 4]))))
    (is (bytes= [1 2 3 4] (decode-hex "4401020304"))))
  (testing "streamed chunks"
    (is (bytes= [1 2 3 4 5] (decode-hex "5F42010243030405FF")))
    (is (cbor-error? {:type ::codec/illegal-chunk-type
                      :data {:stream-type :byte-string
                             :chunk-type :unsigned-integer}}
          (decode-hex "5F42010201FF")))
    (is (cbor-error? {:type ::codec/illegal-stream
                      :data {:stream-type :byte-string}}
          (decode-hex "5F4201025F4100FFFF")))))


(deftest text-strings
  (testing "direct strings"
    (check-roundtrip "" "60")
    (check-roundtrip "a" "6161")
    (check-roundtrip "IETF" "6449455446")
    (check-roundtrip "\"\\" "62225C")
    (check-roundtrip "\u00fc" "62C3BC")
    (check-roundtrip "\u6c34" "63E6B0B4")
    (check-roundtrip "\ud800\udd51" "64F0908591"))
  (testing "streamed chunks"
    (is (= "streaming" (decode-hex "7F657374726561646D696E67FF")))
    (is (cbor-error? {:type ::codec/illegal-chunk-type
                      :data {:stream-type :text-string
                             :chunk-type :negative-integer}}
          (decode-hex "7F6265732100FF")))
    (is (cbor-error? {:type ::codec/illegal-stream
                      :data {:stream-type :text-string}}
          (decode-hex "7F6265737F6161FFFF")))))


(deftest data-arrays
  (testing "encoded size"
    (check-roundtrip [] "80")
    (check-roundtrip [1 2 3] "83010203")
    (check-roundtrip [1 [2 3] [4 5]] "8301820203820405")
    (check-roundtrip (range 1 26) "98190102030405060708090A0B0C0D0E0F101112131415161718181819"))
  (testing "streaming"
    (is (true? (:cbor/streaming (meta (decode-hex "9FFF")))))
    (is (= [] (decode-hex "9FFF")))
    (is (= [1 [2 3] [4 5]] (decode-hex "9F018202039F0405FFFF")))
    (is (= [1 [2 3] [4 5]] (decode-hex "9F01820203820405FF")))
    (is (= [1 [2 3] [4 5]] (decode-hex "83018202039F0405FF")))
    (is (= [1 [2 3] [4 5]] (decode-hex "83019F0203FF820405")))
    (is (= (range 1 26) (decode-hex "9F0102030405060708090A0B0C0D0E0F101112131415161718181819FF")))))


(deftest data-maps
  (testing "encoded size"
    (check-roundtrip {} "A0")
    (check-roundtrip {1 2, 3 4} "A201020304")
    (check-roundtrip {"a" 1, "b" [2 3]} "A26161016162820203")
    (check-roundtrip ["a" {"b" "c"}] "826161A161626163")
    (check-roundtrip {"a" "A", "b" "B", "c" "C", "d" "D", "e" "E"}
                     "A56161614161626142616361436164614461656145"))
  (testing "streaming"
    (is (true? (:cbor/streaming (meta (decode-hex "BFFF")))))
    (is (= {} (decode-hex "BFFF")))
    (is (= {"a" 1, "b" [2 3]} (decode-hex "BF61610161629F0203FFFF")))
    (is (= ["a" {"b" "c"}] (decode-hex "826161BF61626163FF")))
    (is (= {"Fun" true, "Amt" -2} (decode-hex "BF6346756EF563416D7421FF"))))
  (testing "canonical mode"
    (let [codec (cbor/cbor-codec :canonical true)]
      (is (= "A3000861610243000102626263"
             (encoded-hex codec {0 8, "a" 2, (byte-array [0 1 2]) "bc"})))))
  (testing "errors"
    (testing "duplicate key in fixed map"
      (is (cbor-error? {:type ::codec/duplicate-map-key
                        :data {:map {"Fun" true}, :key "Fun"}}
            (decode-hex "A26346756EF56346756EF4"))))
    (testing "duplicate key in streaming map"
      (is (cbor-error? {:type ::codec/duplicate-map-key
                        :data {:map {"Fun" true}, :key "Fun"}}
            (decode-hex "BF6346756EF56346756EF4FF"))))
    (testing "missing value in streaming map"
      (is (cbor-error? {:type ::codec/missing-map-value
                        :data {:map {}, :key "Fun"}}
            (decode-hex "BF6346756EFF"))))))


(deftest set-collections
  (with-codec {}
    (check-roundtrip #{} "D9010280")
    (check-roundtrip #{1 2 3} "D9010283010302"))
  (testing "read handler"
    (is (cbor-error? ::codec/tag-handling-error
          (decode-hex "D90102A10102"))))
  (testing "strict mode"
    (let [codec (cbor/cbor-codec :strict true)]
      (is (cbor-error? ::codec/duplicate-set-entry
            (decode-hex codec "D90102820101")))))
  (testing "canonical mode"
    (let [codec (cbor/cbor-codec :canonical true)]
      (is (= "D90102840018406161820304"
             (encoded-hex codec #{[3 4] 0 64 "a"}))))))


(deftest floating-point-numbers
  (testing "special value encoding"
    (is (= "F90000" (encoded-hex  0.0)))
    (is (= "F90000" (encoded-hex -0.0)))
    (is (= "F97E00" (encoded-hex Float/NaN)))
    (is (= "F97C00" (encoded-hex Float/POSITIVE_INFINITY)))
    (is (= "F9FC00" (encoded-hex Float/NEGATIVE_INFINITY))))
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
    (check-roundtrip (float 100000.0) "FA47C35000")
    (check-roundtrip (float 3.4028234663852886e+38) "FA7F7FFFFF")
    (is (Float/isNaN (decode-hex "FA7FC00000")))
    (is (= Float/POSITIVE_INFINITY (decode-hex "FA7F800000")))
    (is (= Float/NEGATIVE_INFINITY (decode-hex "FAFF800000"))))
  (testing "double-precision"
    (is (instance? Double (decode-hex "FB7FF8000000000000")))
    (check-roundtrip 1.1 "FB3FF199999999999A")
    #_(is (= "FB7E37E43C8800759C" (encoded-hex 1.0e+300)))
    (is (= 1.0e+300 (decode-hex "FB7E37E43C8800759C")))
    (check-roundtrip -4.1 "FBC010666666666666")
    (is (Double/isNaN (decode-hex "FB7FF8000000000000")))
    (is (= Double/POSITIVE_INFINITY (decode-hex "FB7FF0000000000000")))
    (is (= Double/NEGATIVE_INFINITY (decode-hex "FBFFF0000000000000")))))


(deftest simple-values
  (testing "special primitives"
    (check-roundtrip false "F4")
    (check-roundtrip true "F5")
    (check-roundtrip nil "F6")
    (check-roundtrip data/undefined "F7"))
  (testing "generic values"
    (check-roundtrip (data/simple-value 16)  "F0")
    (check-roundtrip (data/simple-value 32)  "F820")
    (check-roundtrip (data/simple-value 255) "F8FF"))
  (testing "reserved codes"
    (is (cbor-error? ::codec/illegal-simple-type
          (encoded-hex (data/simple-value 24))))
    (is (cbor-error? {:type ::codec/illegal-simple-type
                      :data {:code 28}}
          (decode-hex "FC")))
    (is (cbor-error? {:type ::codec/illegal-simple-type
                      :data {:code 29}}
          (decode-hex "FD")))
    (is (cbor-error? {:type ::codec/illegal-simple-type
                      :data {:code 30}}
          (decode-hex "FE")))
    (is (cbor-error? ::codec/unexpected-break
          (decode-hex "FF"))))
  (testing "strict mode"
    (is (cbor-error? ::codec/unknown-simple-value
          (decode-hex (cbor/cbor-codec :strict true) "EF")))))


(deftest tagged-values
  (testing "non-strict mode"
    (is (= (data/tagged-value 11 "a") (decode-hex "CB6161"))))
  (testing "strict mode"
    (let [codec (cbor/cbor-codec :strict true)]
      (is (cbor-error? ::codec/unknown-tag
            (decode-hex codec "CC08")))))
  (testing "handler error"
    (let [handler (fn [_ _] (throw (Exception. "BOOM")))
          codec (cbor/cbor-codec :read-handlers {0 handler})]
      (is (cbor-error? ::codec/tag-handling-error
            (decode-hex codec "C00F")))))
  (testing "unknown types"
    (is (cbor-error? ::codec/unsupported-type
          (encoded-hex (java.util.Currency/getInstance "USD"))))))


(deftest jump-table
  (testing "Positive Integers"
    (dotimes [i 24]
      (is (= i (cbor/decode (byte-array [i])))))
    (is (= 1 (decode-hex "1801")))
    (is (= 2 (decode-hex "190002")))
    (is (= 3 (decode-hex "1A00000003")))
    (is (= 4 (decode-hex "1B0000000000000004")))
    (is (cbor-error? ::codec/illegal-stream
          (decode-hex "1F"))))
  (testing "Negative Integers"
    (dotimes [i 24]
      (is (= (dec (- i)) (cbor/decode (byte-array [(+ 0x20 i)])))))
    (is (= -1 (decode-hex "3800")))
    (is (= -2 (decode-hex "390001")))
    (is (= -3 (decode-hex "3A00000002")))
    (is (= -4 (decode-hex "3B0000000000000003")))
    (is (cbor-error? ::codec/illegal-stream
          (decode-hex "3F"))))
  (testing "Byte Strings"
    (is (bytes= [] (decode-hex "40")))
    (dotimes [i 24]
      (let [bs (vec (range i))
            hex (->> (cons (+ 0x40 i) bs)
                     (map #(format "%02X" %))
                     (apply str))]
        (is (bytes= bs (decode-hex hex)))))
    (is (bytes= [42] (decode-hex "58012A")))
    (is (bytes= [16] (decode-hex "59000110")))
    (is (bytes= [96] (decode-hex "5A0000000160")))
    (is (bytes= [27] (decode-hex "5B00000000000000011B"))))
  (testing "Text Strings"
    (is (= "" (decode-hex "60")))
    (dotimes [i 24]
      (let [string (apply str (repeat i "X"))
            hex (apply str (format "%02X" (+ 0x60 i)) (repeat i "58"))]
        (is (= string (decode-hex hex)))))
    (is (= "X" (decode-hex "780158")))
    (is (= "XX" (decode-hex "7900025858")))
    (is (= "XXX" (decode-hex "7A00000003585858")))
    (is (= "XXXX" (decode-hex "7B000000000000000458585858"))))
  (testing "Arrays"
    (is (= [] (decode-hex "80")))
    (dotimes [i 24]
      (let [nums (vec (repeat i 0))
            hex (apply str (format "%02X" (+ 0x80 i)) (repeat i "00"))]
        (is (= nums (decode-hex hex)))))
    (is (= [0] (decode-hex "980100")))
    (is (= [0] (decode-hex "99000100")))
    (is (= [0] (decode-hex "9A0000000100")))
    (is (= [0] (decode-hex "9B000000000000000100"))))
  (testing "Maps"
    (is (= {} (decode-hex "A0")))
    (dotimes [i 24]
      (let [m (into {} (map (juxt identity identity)) (range i))
            hex (->> (range i)
                     (mapcat (juxt identity identity))
                     (cons (+ 0xA0 i))
                     (map #(format "%02X" %))
                     (apply str))]
        (is (= m (decode-hex hex)))))
    (is (= {0 0} (decode-hex "B8010000")))
    (is (= {0 0} (decode-hex "B900010000")))
    (is (= {0 0} (decode-hex "BA000000010000")))
    (is (= {0 0} (decode-hex "BB00000000000000010000"))))
  (testing "Tagged Values"
    ; 0-4, 27, 30, 32, 35, 37, 39, 55799 are all handled by built-in types.
    ; 5-23
    (doseq [i (range 5 24)]
      (is (= (data/tagged-value i 0) (decode-hex (str (format "%02X" (+ 0xC0 i)) "00")))))
    (is (= (data/tagged-value 5 0) (decode-hex "D80500")))
    (is (= (data/tagged-value 5 0) (decode-hex "D9000500")))
    (is (= (data/tagged-value 5 0) (decode-hex "DA0000000500")))
    (is (= (data/tagged-value 5 0) (decode-hex "DB000000000000000500")))
    (is (cbor-error? ::codec/illegal-stream
          (decode-hex "DF00"))))
  (testing "Simple Values"
    (doseq [v (range 20)]
      (is (= (data/simple-value v) (cbor/decode (byte-array [(+ 0xE0 v)])))))
    (is (false? (decode-hex "F4")))
    (is (true? (decode-hex "F5")))
    (is (nil? (decode-hex "F6")))
    (is (= data/undefined (decode-hex "F7")))
    (is (= (data/simple-value 117) (decode-hex "F875")))
    (is (= 1.5 (decode-hex "F93E00")))
    (is (= (float 100000.0) (decode-hex "FA47C35000")))
    (is (= 1.1 (decode-hex "FB3FF199999999999A"))))
  (testing "Illegal Headers"
    (is (cbor-error? {:type :clj-cbor.header/reserved-info-code
                      :data {:header 157
                             :info 29}}
          (decode-hex "9D")))))
