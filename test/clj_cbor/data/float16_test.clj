(ns clj-cbor.data.float16-test
  (:require
    [clj-cbor.data.float16 :as float16]
    [clojure.test :refer [deftest testing is]]))


(deftest float-decoding
  (testing "constants"
    (is (= 0.0 (float16/decode float16/zero)))
    (is (= Float/POSITIVE_INFINITY (float16/decode float16/positive-infinity)))
    (is (= Float/NEGATIVE_INFINITY (float16/decode float16/negative-infinity)))
    (is (Float/isNaN (float16/decode float16/not-a-number))))
  (testing "examples"
    (is (= 1.0 (float16/decode 2r0011110000000000)))
    (is (= -2.0 (float16/decode 2r1100000000000000)))
    (is (= 65504.0 (float16/decode 2r0111101111111111)))))


(deftest float-encoding
  (testing "constants"
    (is (= float16/zero (float16/encode 0.0)))
    (is (= float16/positive-infinity (float16/encode Float/POSITIVE_INFINITY)))
    (is (= float16/negative-infinity (float16/encode Float/NEGATIVE_INFINITY)))
    (is (= float16/not-a-number (float16/encode Float/NaN))))
  (testing "examples"
    (is (= 2r1000000000000000 (float16/encode -0.0)))
    (is (= 2r0011110000000000 (float16/encode 1.0)))
    (is (= 2r0011110000000001 (float16/encode 1.0009765625)))
    (is (= 2r1100000000000000 (float16/encode -2.0)))
    (is (= 2r0111101111111111 (float16/encode 65504.0)))
    (is (= 2r0011010101010101 (float16/encode 1/3))))
  (testing "edge cases"
    (is (= float16/positive-infinity (float16/encode 65520.0))
        "overflow to positive infinity")
    (is (= float16/negative-infinity (float16/encode -65520.0))
        "underflow to negative infinity")
    (is (= 2r0000000000000010 (float16/encode 0.0000001))
        "subnormal value")
    (is (= 0 (float16/encode 0.000000001))
        "underflow to zero")))
