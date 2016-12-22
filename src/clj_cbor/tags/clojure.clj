(ns clj-cbor.tags.clojure
  "Built-in tag support for the clojure type extensions.

  See:
  - https://github.com/lucas-clemente/cbor-specs/blob/master/id.md"
  (:require
    [clj-cbor.data.model :as data])
  (:import
    (clojure.lang
      Keyword
      Symbol
      TaggedLiteral)))


;; ## Sets

(defn parse-set
  [tag value]
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
  [tag value]
  (when-not (string? value)
    (throw (ex-info (str "Symbols must be tagged strings, got: "
                         (class value))
                    {:tag tag, :value value})))
  (if (= \: (first value))
    (keyword value)
    (symbol value)))



;; ## Tagged Literals

;; Tag 27
;; http://cbor.schmorp.de/generic-object

(defn format-tagged-literal
  [value]
  (data/tagged-value 27 [(str (:tag value)) (:form value)]))


(defn parse-tagged-literal
  [tag value]
  (when-not (and (sequential? value) (= 2 (count value)))
    (throw (ex-info (str "Sets must be tagged 2-element arrays, got: "
                         (class value))
                    {:tag tag, :value value})))
  (tagged-literal (symbol (first value)) (second value)))



;; ## Codec Formatter/Handler Maps

(def clojure-formatters
  "Map of Clojure types to formatting functions."
  {Keyword       format-symbol
   Symbol        format-symbol
   TaggedLiteral format-tagged-literal})


(def clojure-handlers
  "Map of tag handlers to parse Clojure values."
  {13 parse-set
   27 parse-tagged-literal
   39 parse-symbol})
