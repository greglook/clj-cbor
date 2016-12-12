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


;; ## Formatting

(defn format-big-integer
  [^BigInteger value]
  (if (pos? value)
    (data/tagged-value 2 (.toByteArray value))
    (data/tagged-value 3 (.toByteArray (.negate (.add value BigInteger/ONE))))))


(def bignum-formatters
  "Map of bignum types to render as tag 2/3 values."
  {BigInteger format-big-integer
   BigInt (comp format-big-integer biginteger)})



;; ## Parsing

(defn parse-bignum
  [^long tag value]
  ; TODO: assert that value is byte-array
  (let [big-integer (BigInteger. ^bytes value)]
    (case tag
      2 (bigint big-integer)
      3 (bigint (.negate (.add big-integer BigInteger/ONE))))))



(def bignum-handlers
  "Map of tag handlers to parse bignum values."
  {2 parse-bignum
   3 parse-bignum})
