(ns clj-cbor.tags.clojure
  "Built-in tag support for the clojure type extensions.

  See:
  - https://github.com/lucas-clemente/cbor-specs/blob/master/id.md"
  (:require
    [clj-cbor.data.model :as data])
  (:import
    (clojure.lang
      Keyword
      Symbol)))


;; ## Sets

(defn parse-set
  [^long tag value]
  (when-not (sequential? value)
    (throw (ex-info (str "Sets must be tagged arrays, got: "
                         (class value))
                    {:tag tag, :value value})))
  (set value))



;; ## Symbols & Keywords

(defn format-symbol
  [value]
  (data/tagged-value 39 (str value)))


(defn parse-symbol
  [^long tag value]
  (when-not (string? value)
    (throw (ex-info (str "Symbols must be tagged strings, got: "
                         (class value))
                    {:tag tag, :value value})))
  (if (= \: (first value))
    (keyword value)
    (symbol value)))



;; ## Codec Formatter/Handler Maps

(def clojure-formatters
  "Map of Clojure types to formatting functions."
  {Keyword format-symbol
   Symbol  format-symbol})


(def clojure-handlers
  "Map of tag handlers to parse Clojure values."
  {13 parse-set
   39 parse-symbol})
