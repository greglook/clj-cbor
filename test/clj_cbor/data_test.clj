(ns clj-cbor.data-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.decoder :as cd]))


(deftest major-types
  (testing "unsigned integers"
    (is (= (cd/decode-initial 2r00000000) [:unsigned-integer 0]))
    (is (= (cd/decode-initial 2r00010000) [:unsigned-integer 16]))
    (is (= (cd/decode-initial 2r00011000) [:unsigned-integer :uint8]))))
