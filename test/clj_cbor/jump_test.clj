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

(deftest jump-table
  (testing "negative integers"
    (let [tbl jump/jump-decoder-table]
      (doseq [idx (range 0x20 0x38)]
        (is (= ((aget tbl idx) nil nil) (dec (- (- idx 0x20))))))
      (are [idx bs target]
          (let [dis (->data-input bs)]
            (is (= ((aget tbl idx) nil dis) target)))
        0x38 [100] -101
        0x39 [1 1] -258
        0x3a [1 1 1 1] -16843010
        0x3b (repeat 8 1) -72340172838076674)))
  (testing "byte strings"
    (let [tbl jump/jump-decoder-table]
      (doseq [idx (range 0x40 0x58)
              :let [bs (repeat (- idx 0x40) 1)
                    dis (->data-input bs)
                    entry-fn (aget tbl idx)]]
        (is (= (seq (entry-fn nil dis)) (seq bs))))
      (are [idx bs target]
        (let [dis (->data-input bs)
              entry-fn (aget tbl idx)]
          (is (= (seq (entry-fn nil dis)) (seq target))))
        ;; byte
        0x58 (concat [100] (repeat 100 1)) (repeat 100 1)
        ;; short
        0x59 (concat [0 100] (repeat 100 1)) (repeat 100 1)
        ;; int
        0x5a (concat [0 0 0 100] (repeat 100 1)) (repeat 100 1)
        ;; long
        0x5b (concat (repeat 7 0) [100] (repeat 100 1)) (repeat 100 1))))
  (testing "utf8 string"
    (let [tbl jump/jump-decoder-table]
      (doseq [idx (range 0x60 0x78)
              :let [bs (repeat (- idx 0x60) (int \a))
                    dis (->data-input bs)
                    entry-fn (aget tbl idx)]]
        (is (= (entry-fn nil dis) (apply str (repeat (count bs)\a)))))
      (are [idx pad len]
          (let [bs (concat (repeat pad 0) [len] (repeat len (int \a)))
                dis (->data-input bs)
                entry-fn (aget tbl idx)]
            (is (= (entry-fn nil dis) (apply str (repeat len \a)))))
        ;; byte [no padding]
        0x78 0 25
        ;; short [1 byte padding]
        0x79 1 25
        0x7a 3 25
        0x7b 7 25)))
  (testing "arrays"
    (let [tbl jump/jump-decoder-table]
      (doseq [idx (range 0x80 0x98)
              :let [entry-fn (aget tbl idx)
                    dis (->data-input (repeat 24 1))
                    coll (repeat (- idx 0x80) :a)
                    decoder (reify decoder/Decoder
                              (read-value* [_ _ _]
                                :a))]]
        (is (= (seq (entry-fn decoder dis)) (seq coll))))
      (are [idx pad]
          ;; padding for uint width, array length as 1 byte, dummy bytes for each entry
          (let [dis (->data-input (concat (repeat pad 0) [100] (repeat 100 0)))
                entry-fn (aget tbl idx)
                coll (repeat 100 :a)
                decoder (reify decoder/Decoder
                          (read-value* [_ _ _] :a))]
            (is (= (seq (entry-fn decoder dis)) (seq coll))))
        ;; byte
        0x98 0
        ;; short
        0x99 1
        ;; int
        0x9a 3
        ;; long
        0x9b 7))
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
      0x80 [] []))
  (testing "dictionary/map"
    (let [tbl jump/jump-decoder-table]
      (doseq [n (range 0 24)
              :let [entry-fn (aget tbl (+ 0xa0 n))
                    m (into {} (for [idx (range n)] [idx idx]))
                    coll (flatten (seq m))
                    iter (.iterator coll)
                    dis (->data-input (repeat (* 2 (count m)) 1))
                    decoder (reify decoder/Decoder
                              (read-value* [_ _ _]
                                (.next iter)))]]
        (is (= (entry-fn decoder dis) m))))))
