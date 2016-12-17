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
  ; TODO: assert that value is byte-array
  (let [big-integer (BigInteger. value)]
    (case tag
      2 (bigint big-integer)
      3 (bigint (.negate (.add big-integer BigInteger/ONE))))))



;; ## Codec Formatter/Handler Maps

(def number-formatters
  "Map of bignum types to render as tag 2/3 values."
  {BigInteger format-big-int
   BigInt format-big-int})


(def number-handlers
  "Map of tag handlers to parse bignum values."
  {2 parse-big-int
   3 parse-big-int})
