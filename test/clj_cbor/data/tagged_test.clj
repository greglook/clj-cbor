(ns clj-cbor.data.tagged-test
  (:require
    [clj-cbor.data.tagged :refer [->TaggedValue]]
    [clojure.test :refer [deftest testing is]]))


(deftest tagged-values
  (let [uri-value (->TaggedValue 32 "https://mvxcvi.com/" nil)
        ratio-value (->TaggedValue 30 [1 3] nil)]
    (testing "representation"
      (is (= "32(https://mvxcvi.com/)" (str uri-value)))
      (is (= "30([1 3])" (str ratio-value))))
    (testing "equality"
      (is (= uri-value uri-value)
          "should be reflexive")
      (is (= ratio-value (->TaggedValue 30 [1 3] nil))
          "different instances of the same value should be equal")
      (is (not= ratio-value (->TaggedValue 30 [1 4] nil))
          "different values of the same tag should not be equal")
      (is (not= uri-value ratio-value)
          "different simple values should not be equal")
      (is (not= uri-value :foo)
          "different types should not be equal"))
    (testing "hash code"
      (is (integer? (hash uri-value)))
      (is (= (hash uri-value) (hash uri-value))
          "should be stable")
      (is (= (hash ratio-value) (hash (->TaggedValue 30 [1 3] nil)))
          "different instances of the same value should have the same hash")
      (is (not= (hash uri-value) (hash ratio-value))
          "different simple values should have different hashes"))
    (testing "metadata"
      (is (nil? (meta uri-value)))
      (is (= uri-value (vary-meta uri-value assoc :x 123))
          "should not affect equality")
      (is (= (hash ratio-value) (hash (vary-meta ratio-value assoc :y true)))
          "should not affect hash code")
      (is (= {:x 123} (meta (vary-meta uri-value assoc :x 123)))
          "metadata is preserved"))))
