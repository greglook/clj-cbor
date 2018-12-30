(ns clj-cbor.codec-test
  "Decoding tests. Test examples are from RFC 7049 Appendix A."
  (:require
   [clojure.test :refer :all]
   [clj-cbor.core :as cbor]
   [clj-cbor.data.core :as data]
   [clj-cbor.test-utils :refer :all]
   [clj-cbor.jump :as jump]))


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

(defn -unsigned-integers []
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
    (is (cbor-error? :clj-cbor.codec/illegal-stream
          (decode-hex "1F00")))))

(deftest unsigned-integers
  (with-codec {}
    (-unsigned-integers))
  (with-codec {:jump-table (jump/jump-decoder-table)}
    (-unsigned-integers)))

(defn -negative-integers []
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
    (is (cbor-error? :clj-cbor.codec/illegal-stream
          (decode-hex "3F00")))))


(def jump-table-opts
  {:jump-table (jump/jump-decoder-table)})

(deftest negative-integers
  (with-codec {}
    (-negative-integers))
  (testing "jump decoder"
    (with-codec jump-table-opts
      (-negative-integers))))


(defn -byte-strings []
  (testing "direct bytes"
    (is (= "40" (encoded-hex (byte-array 0))))
    (is (bytes= [] (decode-hex "40")))
    (is (= "4401020304" (encoded-hex (byte-array [1 2 3 4]))))
    (is (bytes= [1 2 3 4] (decode-hex "4401020304"))))
  (testing "streamed chunks"
    (is (bytes= [1 2 3 4 5] (decode-hex "5F42010243030405FF")))
    (is (cbor-error? {:type :clj-cbor.codec/illegal-chunk-type
                      :data {:stream-type :byte-string
                             :chunk-type :unsigned-integer}}
          (decode-hex "5F42010201FF")))
    (is (cbor-error? {:type :clj-cbor.codec/illegal-stream
                      :data {:stream-type :byte-string}}
          (decode-hex "5F4201025F4100FFFF")))))


(deftest byte-strings
  (with-codec {}
    (-byte-strings))
  (with-codec jump-table-opts
    (-byte-strings)))


(defn -text-strings []
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
    (is (cbor-error? {:type :clj-cbor.codec/illegal-chunk-type
                      :data {:stream-type :text-string
                             :chunk-type :negative-integer}}
          (decode-hex "7F6265732100FF")))
    (is (cbor-error? {:type :clj-cbor.codec/illegal-stream
                      :data {:stream-type :text-string}}
          (decode-hex "7F6265737F6161FFFF")))))

(deftest text-strings
  (with-codec {}
    (-text-strings))
  (with-codec jump-table-opts
    (-text-strings)))


(defn -data-arrays []
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

(deftest data-arrays
  (with-codec {}
    (-data-arrays))
  (with-codec jump-table-opts
    (-data-arrays)))


(defn -data-maps []
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
    (is (cbor-error? {:type :clj-cbor.codec/missing-map-value
                      :data {:map {}, :key "Fun"}}
          (decode-hex "BF6346756EFF")))
    (is (cbor-error? {:type :clj-cbor.codec/duplicate-map-key
                      :data {:map {"Fun" true}, :key "Fun"}}
          (decode-hex "A26346756EF56346756EF4FF")))))

(deftest data-maps
  (with-codec {}
    (-data-maps))
  (with-codec jump-table-opts
    (-data-maps)))

(defn -set-collections []
  (with-codec {:set-tag 258}
    (check-roundtrip #{} "D9010280")
    (check-roundtrip #{1 2 3} "D9010283010302"))
  (testing "read handler"
    (is (cbor-error? :clj-cbor.codec/tag-handling-error
          (decode-hex "D90102A10102"))))
  (testing "strict mode"
    (let [codec (cbor/cbor-codec :strict true)]
      (is (cbor-error? :clj-cbor.codec/duplicate-set-entry
            (decode-hex codec "D90102820101")))))
  (testing "canonical mode"
    (let [codec (cbor/cbor-codec :canonical true)]
      (is (= "D90102840018406161820304"
             (encoded-hex codec #{[3 4] 0 64 "a"}))))))

(deftest set-collections
  (with-codec {}
    (-set-collections))
  (with-codec jump-table-opts
    (-set-collections)))


(defn -floating-point-numbers []
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

(deftest floating-point-numbers
  (with-codec {}
    (-floating-point-numbers))
  (with-codec jump-table-opts
    (-floating-point-numbers)))


(defn -simple-values []
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
    (is (cbor-error? :clj-cbor.codec/illegal-simple-type
          (encoded-hex (data/simple-value 24))))
    (is (cbor-error? {:type :clj-cbor.codec/illegal-simple-type
                      :data {:code 28}}
          (decode-hex "FC")))
    (is (cbor-error? {:type :clj-cbor.codec/illegal-simple-type
                      :data {:code 29}}
          (decode-hex "FD")))
    (is (cbor-error? {:type :clj-cbor.codec/illegal-simple-type
                      :data {:code 30}}
          (decode-hex "FE")))
    (is (cbor-error? :clj-cbor.codec/unexpected-break
          (decode-hex "FF"))))
  (testing "strict mode"
    (is (cbor-error? :clj-cbor.codec/unknown-simple-value
          (decode-hex (cbor/cbor-codec :strict true) "EF")))))

(deftest simple-values []
  (with-codec {}
    (-simple-values))
  (with-codec jump-table-opts
    (-simple-values)))

(defn -tagged-values []
  (testing "non-strict mode"
    (is (= (data/tagged-value 11 "a") (decode-hex "CB6161"))))
  (testing "strict mode"
    (let [codec (cbor/cbor-codec :strict true)]
      (is (cbor-error? :clj-cbor.codec/unknown-tag
            (decode-hex codec "CC08")))))
  (testing "handler error"
    (let [handler (fn [t v] (throw (Exception. "BOOM")))
          codec (cbor/cbor-codec :read-handlers {0 handler})]
      (is (cbor-error? :clj-cbor.codec/tag-handling-error
            (decode-hex codec "C00F")))))
  (testing "unknown types"
    (is (cbor-error? :clj-cbor.codec/unsupported-type
          (encoded-hex (java.util.Currency/getInstance "USD"))))))

(deftest tagged-values
  (with-codec {}
    (-tagged-values))
  (with-codec jump-table-opts
    (-tagged-values)))
