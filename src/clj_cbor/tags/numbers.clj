(ns clj-cbor.tags.numbers
  "Built-in tag support for the number extensions in RFC 7049. See section
  2.4.2."
  (:require
    [clj-cbor.data.model :as data])
  (:import
    (clojure.lang
      BigInt
      Ratio)
    (java.math
      BigDecimal
      BigInteger)))


;; ## Bignums

;; Tag 2, 3
;; RFC 7049 Section 2.4.2

(defn format-big-int
  [value]
  (let [big-integer (biginteger value)]
    (if (pos? big-integer)
      (data/tagged-value 2 (.toByteArray big-integer))
      (data/tagged-value 3 (-> big-integer
                               (.add BigInteger/ONE)
                               (.negate)
                               (.toByteArray))))))


(defn parse-big-int
  [^long tag ^bytes value]
  (when-not (data/bytes? value)
    (throw (ex-info (str "Bignums must be represented as a tagged byte array, got: "
                         (class value))
                    {:tag tag, :value value})))
  (let [big-integer (BigInteger. value)]
    (case tag
      2 (bigint big-integer)
      3 (bigint (.negate (.add big-integer BigInteger/ONE))))))



;; ## Decimal Fractions

;; Tag 4
;; RFC 7049 Section 2.4.3

(defn format-big-decimal
  [^BigDecimal value]
  (let [exponent (.scale value)
        mantissa (.unscaledValue value)]
    (data/tagged-value 4 [(- exponent) mantissa])))


(defn parse-big-decimal
  [^long tag value]
  (when-not (and (sequential? value) (= 2 (count value)))
    (throw (ex-info (str "Decimal fractions must be represented with a two-element array, got: "
                         (pr-str value))
                    {:tag tag, :value value})))
  (let [[exponent mantissa] value]
    (BigDecimal. (biginteger mantissa) (int (- exponent)))))



;; ## Bigfloats

;; Tag 5
;; RFC 7049 Section 2.4.3

;; Not Supported



;; ## Rationals

;; Tag 30
;; http://peteroupc.github.io/CBOR/rational.html

(defn format-rational
  [value]
  (data/tagged-value 30 [(numerator value) (denominator value)]))


(defn parse-rational
  [^long tag value]
  (when-not (and (sequential? value) (= 2 (count value)))
    (throw (ex-info (str "Rational numbers must be represented with a two-element array, got: "
                         (pr-str value))
                    {:tag tag, :value value})))
  (let [[numerator denominator] value]
    (Ratio. (biginteger numerator) (biginteger denominator))))



;; ## Codec Formatter/Handler Maps

(def number-formatters
  "Map of number types to formatting functions."
  {BigInt     format-big-int
   BigInteger format-big-int
   BigDecimal format-big-decimal
   Ratio      format-rational})


(def number-handlers
  "Map of tag handlers to parse number values."
  { 2 parse-big-int
    3 parse-big-int
    4 parse-big-decimal
   30 parse-rational})
