(ns clj-cbor.core-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.codec :as codec]
    [clj-cbor.test-utils :refer :all]))


(deftest codec-construction
  (let [codec (cbor/cbor-codec)]
    (is (satisfies? codec/Encoder codec))
    (is (satisfies? codec/Decoder codec)))
  (let [codec (cbor/cbor-codec
                {:read-handlers {0 :x}
                 :write-handlers {Long :y}})]
    (is (satisfies? codec/Encoder codec))
    (is (satisfies? codec/Decoder codec))
    (is (= {0 :x} (:read-handlers codec)))
    (is (= {Long :y} (:write-handlers codec))))
  (let [codec (cbor/cbor-codec
                :read-handlers {0 :x}
                :write-handlers {Long :y})]
    (is (satisfies? codec/Encoder codec))
    (is (satisfies? codec/Decoder codec))
    (is (= {0 :x} (:read-handlers codec)))
    (is (= {Long :y} (:write-handlers codec)))))


(deftest eof-handling
  (testing "sequence of values"
    (is (= (list :a 123 true "foo") (decode-hex-all cbor/default-codec "D827623A61187BF563666F6F"))))
  (testing "interrupted data"
    (is (cbor-error? :clj-cbor.codec/end-of-input
          (decode-hex "D827623A61187BF563666F")))))
