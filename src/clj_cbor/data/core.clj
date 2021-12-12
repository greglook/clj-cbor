(ns clj-cbor.data.core
  "Type definitions and keyword identifiers for CBOR data types."
  (:require
    [clj-cbor.data.simple :as simple]
    [clj-cbor.data.tagged :as tagged])
  (:import
    (clj_cbor.data.simple
      SimpleValue)
    (clj_cbor.data.tagged
      TaggedValue)))


;; ## Simple Values

(def undefined
  "Base singleton undefined value."
  (simple/->Undefined nil))


(defn simple-value
  "Constructs a simple type for the given number."
  [n]
  (when (or (neg? n) (< 255 n))
    (throw (IllegalArgumentException.
             "Simple value codes must be between 0 and 255")))
  (simple/->SimpleValue n nil))


(defn simple-value?
  "Predicate which tests whether `x` is a simple CBOR value."
  [x]
  (instance? SimpleValue x))


;; ## Tagged Values

(defn tagged-value
  "Constructs a tagged value."
  [tag value]
  (tagged/->TaggedValue tag value nil))


(defn tagged-value?
  "Predicate which tests whether `x` is a CBOR tagged value."
  [x]
  (instance? TaggedValue x))


(def set-tag
  "Tag code used to identify sets of unique values. Hard-coded here to support
  canonical encoding."
  258)
