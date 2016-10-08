(ns clj-cbor.types-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.types :as ct]))


(deftest major-types
  (testing "unsigned integers"
    (is (= (ct/decode-initial 2r00000000) [:unsigned-integer 0]))
    (is (= (ct/decode-initial 2r00010000) [:unsigned-integer 16]))
    (is (= (ct/decode-initial 2r00011000) [:unsigned-integer :uint8]))))
