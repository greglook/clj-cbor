(ns clj-cbor.core
  (:require
    [clj-cbor.data :as data])
  (:import
    (clj_cbor.data
      SimpleValue
      Undefined)))


(defn decode
  [content]
  nil)


(defn undefined?
  "Predicate which returns true if `x` is a CBOR undefined value."
  [x]
  (instance? Undefined x))
