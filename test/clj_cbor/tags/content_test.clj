(ns clj-cbor.tags.content-test
  (:require
    [clj-cbor.core :as cbor]
    [clj-cbor.tags.content :refer :all]
    [clj-cbor.test-utils :refer :all]
    [clojure.test :refer :all]))


(deftest self-described
  (is (= "D9D9F70F" (encoded-hex (cbor/self-describe 15))))
  (is (= 15 (decode-hex cbor/default-codec "D9D9F70F"))))
