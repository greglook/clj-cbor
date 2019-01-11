(ns clj-cbor.generative-test
  "Decoding tests. Test examples are from RFC 7049 Appendix A."
  (:require
    [clojure.test :refer :all]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clj-cbor.core :as cbor]
    [clj-cbor.data.core :as data]
    [clj-cbor.test-utils :refer :all])
  (:import
    (java.util
      Collection
      List
      Map
      Set)
    java.util.regex.Pattern))


(defmulti equivalent?
  (fn [a b] (class a)))


(defmethod equivalent? :default
  [a b]
  (= a b))


(defmethod equivalent? (class (byte-array 0))
  [a b]
  (bytes= a b))


(defmethod equivalent? Character
  [a b]
  (= (str a) (str b)))


(defmethod equivalent? Pattern
  [a b]
  (= (str a) (str b)))


(defmethod equivalent? Number
  [a b]
  (if (Double/isNaN a)
    (Double/isNaN b)
    (= a b)))


(defmethod equivalent? List
  [a b]
  (and (instance? List b)
       (= (count a) (count b))
       (every? true? (map equivalent? a b))))


(defmethod equivalent? Set
  [a b]
  (and (instance? Set b)
       (= (count a) (count b))
       (every? #(equivalent? % (get b %)) a)))


(defmethod equivalent? Map
  [a b]
  (and (instance? Map b)
       (= (count a) (count b))
       (loop [a a
              b b]
         (if-let [[k v] (first a)]
           (if-let [[match-k match-v]
                    (first (filter (comp (partial equivalent? k) key) b))]
             (if (equivalent? v match-v)
               (recur (dissoc a k) (dissoc b match-k))
               false)
             false)
           (empty? b)))))


(defspec ^:generative round-trip-equivalence 100
  (prop/for-all [x gen/any-printable]
    (equivalent? x (cbor/decode (cbor/encode x)))))
