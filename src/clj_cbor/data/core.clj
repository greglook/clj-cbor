(ns clj-cbor.data.core
  "Type definitions and keyword identifiers for CBOR data types."
  (:require
    [clj-cbor.data.float16 :as float16]
    [clj-cbor.data.simple :as simple]
    [clj-cbor.data.tagged :as tagged])
  (:import
    (clj_cbor.data.simple
      SimpleValue
      Undefined)
    clj_cbor.data.tagged.TaggedValue))


;; ## Byte Arrays

;; Sorting is performed on the bytes of the representation of the key data
;; items without paying attention to the 3/5 bit splitting for major types.
;; The sorting rules are:
;;
;; * If two keys have different lengths, the shorter one sorts
;;   earlier;
;;
;; * If two keys have the same length, the one with the lower value
;;   in  (byte-wise) lexical order sorts earlier.

(defn compare-bytes
  "Returns a negative number, zero, or a positive number when `x` is 'less
  than', 'equal to', or 'greater than' `y`."
  [x y]
  (cond
    (< (count x) (count y)) -1
    (> (count x) (count y))  1
    :else
    (if-let [[x' y'] (some
                       (fn [[x' y' :as data]]
                         (when (not= x' y')
                           data))
                       (map vector x y))]
      (if (< x' y') -1 1)
      0)))


(let [byte-array-class (class (byte-array 0))]
  (defn bytes?
    "Predicate which returns true if `x` is a byte-array."
    [x]
    (instance? byte-array-class x)))



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
