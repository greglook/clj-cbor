(ns clj-cbor.core-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.codec :as codec]
    [clj-cbor.test-utils :refer :all])
  (:import
    java.io.File
    java.time.Instant))


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


(deftest multi-value-coding
  (is (= [true 123 :abc] (cbor/decode-all (cbor/encode-all [true 123 :abc])))))


(deftest eof-handling
  (testing "sequence of values"
    (is (= (list :a 123 true "foo") (decode-hex-all cbor/default-codec "D827623A61187BF563666F6F"))))
  (testing "interrupted data"
    (is (cbor-error? :clj-cbor.codec/end-of-input
          (decode-hex-all "D827623A61187BF563666F")))))


(deftest slobber-utils
  (let [file (File/createTempFile "clj-cbor.core-test" ".cbor")
        data-a [3 4.7 true "foo" :bar 'baz/qux {(Instant/ofEpochMilli 1234567890) {:foo 123.456M}}]
        data-b {:foo "bar", :abc 123}]
    (is (= 60 (cbor/spit file data-a)))
    (is (= 21 (cbor/spit file data-b :append true)))
    (is (= 81 (.length file)))
    (is (= data-a (cbor/slurp file)))
    (is (= [data-a data-b] (cbor/slurp-all file)))
    (.delete file)
    (is (= 81 (cbor/spit-all file [data-b data-a])))
    (is (= [data-b data-a] (cbor/slurp-all file)))))
