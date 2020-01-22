(ns clj-cbor.core-test
  (:require
    [clj-cbor.codec :as codec]
    [clj-cbor.core :as cbor]
    [clj-cbor.test-utils :refer :all]
    [clojure.test :refer :all])
  (:import
    (java.io
      ByteArrayInputStream
      File)
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


(deftest encode-output-guard
  (is (thrown-with-msg? IllegalArgumentException #"coerce argument to an OutputStream"
        (cbor/encode cbor/default-codec :not-an-output-stream 123))))


(deftest multi-value-coding
  (is (= [true 123 :abc] (cbor/decode-seq (cbor/encode-seq [true 123 :abc])))))


(deftest eof-handling
  (testing "sequence of values"
    (is (= (list :a 123 true "foo") (decode-hex-all cbor/default-codec "D827623A61187BF563666F6F"))))
  (testing "interrupted data"
    (is (cbor-error? :clj-cbor.codec/end-of-input
          (decode-hex-all "D827623A61187BF563666F")))))


(deftest lazy-decoding
  (let [data (cbor/encode-seq [:a :b :c])
        input (ByteArrayInputStream. data)]
    (is (= :a (cbor/decode input)))
    (is (= :b (cbor/decode input)))
    (is (= [:c] (cbor/decode-seq input)))))


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
