(ns clj-cbor.data.simple-test
  (:require
    [clj-cbor.data.simple :refer [->Undefined ->SimpleValue]]
    [clojure.test :refer [deftest testing is]]))


(deftest undefined-values
  (let [undefined (->Undefined nil)]
    (testing "representation"
      (is (= "undefined" (str undefined))))
    (testing "equality"
      (is (= undefined undefined))
      (is (= undefined (->Undefined nil))
          "all undefined values should be equal")
      (is (not= undefined :foo)))
    (testing "hash code"
      (is (integer? (hash undefined)))
      (is (= (hash undefined) (hash (->Undefined nil)))
          "all undefined values should have the same hash"))
    (testing "metadata"
      (is (nil? (meta undefined)))
      (is (= undefined (vary-meta undefined assoc :x 123))
          "metadata does not affect equality")
      (is (= {:x 123} (meta (vary-meta undefined assoc :x 123)))
          "metadata is preserved"))))


(deftest simple-values
  (let [simple24 (->SimpleValue 24 nil)
        simple64 (->SimpleValue 64 nil)]
    (testing "representation"
      (is (= "simple(24)" (str simple24)))
      (is (= "simple(64)" (str simple64))))
    (testing "equality"
      (is (= simple24 simple24)
          "should be reflexive")
      (is (= simple64 simple64)
          "should be reflexive")
      (is (= simple24 (->SimpleValue 24 nil))
          "different instances of the same value should be equal")
      (is (not= simple24 simple64)
          "different simple values should not be equal")
      (is (not= simple64 :foo)
          "different types should not be equal"))
    (testing "hash code"
      (is (integer? (hash simple24)))
      (is (= (hash simple24) (hash simple24))
          "should be stable")
      (is (= (hash simple24) (hash (->SimpleValue 24 nil)))
          "different instances of the same value should have the same hash")
      (is (not= (hash simple24) (hash simple64))
          "different simple values should have different hashes"))
    (testing "comparable"
      (is (zero? (compare simple24 simple24))
          "identical instances should compare the same")
      (is (zero? (compare simple24 (->SimpleValue 24 nil)))
          "different instances of the same value should compare the same")
      (is (neg? (compare simple24 simple64))
          "lower numbered values should sort earlier")
      (is (pos? (compare simple64 simple24))
          "higher numbered values should sort later"))
    (testing "metadata"
      (is (nil? (meta simple24)))
      (is (= simple24 (vary-meta simple24 assoc :x 123))
          "should not affect equality")
      (is (= (hash simple24) (hash (vary-meta simple24 assoc :y true)))
          "should not affect hash code")
      (is (zero? (compare simple24 (vary-meta simple24 assoc :foo :abc)))
          "should not affect comparison")
      (is (= {:x 123} (meta (vary-meta simple24 assoc :x 123)))
          "metadata is preserved"))))
