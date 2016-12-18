(ns clj-cbor.tags.numbers
  "Built-in tag support for the number extensions in RFC 7049. See section
  2.4.2."
  (:require
    [clj-cbor.data.model :as data])
  (:import
    clojure.lang.BigInt
    (java.math
      BigDecimal
      BigInteger)))


;; ## Bignums

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

(defn format-big-decimal
  [^BigDecimal value]
  (let [exponent (.scale value)
        mantissa (.unscaledValue value)]
    (data/tagged-value 4 [(- exponent) mantissa])))


(defn parse-big-decimal
  [^long tag value]
  (when (not= tag 4)
    (throw (ex-info (str "Decimal fractions must be represented with tag 4, got: "
                         tag)
                    {:tag tag, :value value})))
  (let [[exponent mantissa] value]
    (BigDecimal. (biginteger mantissa) (- exponent))))



;; ## Bigfloats

;; Not Supported



;; ## Codec Formatter/Handler Maps

(def number-formatters
  "Map of bignum types to render as tag 2/3 values."
  {BigInt     format-big-int
   BigInteger format-big-int
   BigDecimal format-big-decimal})


(def number-handlers
  "Map of tag handlers to parse bignum values."
  {2 parse-big-int
   3 parse-big-int
   4 parse-big-decimal})
