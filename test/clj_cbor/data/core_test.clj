(ns clj-cbor.data.core-test
  (:require
    [clj-cbor.data.core :as data]
    [clj-cbor.test-utils :refer :all]
    [clojure.test :refer :all]))


(deftest simple-value-construction
  (is (data/simple-value? (data/simple-value 21)))
  (is (thrown? IllegalArgumentException
        (data/simple-value -1)))
  (is (thrown? IllegalArgumentException
        (data/simple-value 256))))
