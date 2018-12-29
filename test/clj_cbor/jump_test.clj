(ns clj-cbor.jump-test
  (:require [clj-cbor.jump :as jump]
            [clj-cbor.test-utils :refer :all]
            [clojure.test :refer :all]
            [clj-cbor.decoder :as decoder]))

(deftest uint-expr
  (is (= '(.readUnsignedShort dis)
         (jump/uint-expr :short 'dis)))
  (is (= '(.readUnsignedByte dis)
         (jump/uint-expr :byte 'dis)))
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

(defn test-map-entry [tbl-idx num-byte-padding map-len]
  ;; create enough input to represent map
  (let [tbl jump/jump-decoder-table
        bs (concat (repeat num-byte-padding 0)
                   [map-len]
                   (repeat (* 2 map-len) 0))
        m (into {} (for [i (range map-len)] [i i]))
        coll (flatten (seq m))
        iter (.iterator coll)
        dis (->data-input bs)
        decoder (reify decoder/Decoder
                  (read-value* [_ _ _]
                    (.next iter)))
        entry-fn (aget tbl tbl-idx)]
    (is (= (entry-fn decoder dis) m))))

(defn test-array-entry [tbl-idx num-pad coll]
  (let [iter (.iterator coll)
        ;; this exists so bytes can be read
        bs (concat (repeat num-pad 0) [(count coll)] (repeat (count coll) 0))
        dis (->data-input bs)
        ;; this will 'mock' the array entry values
        decoder (reify decoder/Decoder
                  (read-value* [_ _ _]
                    (.next iter)))
        entry-fn (aget jump/jump-decoder-table tbl-idx)]
    (is (= coll (entry-fn decoder dis)))))

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
        ;; byte, short, int, long
        0x58 (concat [100] (repeat 100 1)) (repeat 100 1)
        0x59 (concat [0 100] (repeat 100 1)) (repeat 100 1)
        0x5a (concat [0 0 0 100] (repeat 100 1)) (repeat 100 1)
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
    (doseq [idx (range 24)]
      (test-array-entry (+ 0x80 idx) 0 (range idx)))
    (are [idx pad]
        (test-array-entry idx pad (range 100))
      ;; byte, short, int, long
      0x98 0
      0x99 1
      0x9a 3
      0x9b 7)
    (are [idx coll]
      (test-array-entry idx 0 coll)
      0x83 ["a" "b" "c"]
      0x80 []))
  (testing "dictionary/map"
    (let [tbl jump/jump-decoder-table]
      (doseq [n (range 0 24)]
        (test-map-entry (+ n 0xa0) 0 n))
      (are [idx pad]
        (do (test-map-entry idx pad 100)
            (test-map-entry idx pad 50))
        ;; byte, short, int, long
        0xb8 0
        0xb9 1
        0xba 3
        0xbb 7))))
