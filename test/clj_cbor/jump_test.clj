(ns clj-cbor.jump-test
  (:require [clj-cbor.jump :as jump]
            [clj-cbor.test-utils :refer :all]
            [clojure.test :refer :all]
            [clj-cbor.decoder :as decoder]))

(deftest uint-expr
  (is (= '(.readUnsignedShort dis)
         (jump/uint-expr :short 'dis)))
  (is (= '(clojure.core/bit-and (.readInt dis) 0xFFFFFFFF)
         (jump/uint-expr :int 'dis))))


(deftest gen-entry
  (are [int-type bs target]
      (let [f (jump/gen-entry int-type decoder input val (inc val))]
        (= target (f nil (->data-input bs))))
    0 nil 1
    :byte [10] 11
    :short [1 1] 258
    :int [1 1 1 1] 16843010
    :long [0 0 0 0 0 0 1 1] 258))

(deftest jump-table-fixed-size
  (testing "negative integers"
    (let [tbl jump/jump-decoder-table]
      (are [idx target]
        (= ((aget tbl idx) nil nil) target)
        0x20 -1
        0x21 -2)))
  (testing "arrays"
    (are [idx coll target]
      (let [iter (.iterator coll)
            ;; this exists so bytes can be read
            dis (->data-input (repeat (count coll) 1))
            tbl jump/jump-decoder-table
            decoder (reify decoder/Decoder
                      (read-value* [_ _ _]
                        (.next iter)))
            entry-fn (aget tbl idx)]
        (is (= target (entry-fn decoder dis))))
      0x83 ["a" "b" "c"] ["a" "b" "c"]
      0x80 [] [])))
