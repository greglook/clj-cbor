(ns clj-cbor.tags.clojure
  "Read and write handler support for Clojure types.

  Keywords and symbols are represented using tag 39 ('identifier') applied to
  the string version of the value. This adds three bytes to the size of the
  identifier itself for the header, tag code, and string header. Keywords are
  symbols whose first character is a colon (:).

  Tagged literals are represented using tag 27 ('generic object') applied to an
  array containing two elements. The first element is the string version of the
  EDN tag symbol and the second is the tagged literal form.

  See:
  - https://github.com/lucas-clemente/cbor-specs/blob/master/id.md
  - http://cbor.schmorp.de/generic-object
  "
  (:require
    [clj-cbor.data.model :as data])
  (:import
    (clojure.lang
      Keyword
      Symbol
      TaggedLiteral)))


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
    (keyword (subs value 1))
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
    (throw (ex-info (str "Sets must be tagged two-element arrays, got: "
                         (class value))
                    {:tag tag, :value value})))
  (tagged-literal (symbol (first value)) (second value)))



;; ## Codec Formatter/Handler Maps

(def clojure-write-handlers
  "Map of Clojure types to write handler functions."
  {Keyword       format-symbol
   Symbol        format-symbol
   TaggedLiteral format-tagged-literal})


(def clojure-read-handlers
  "Map of tag codes to read handlers to parse Clojure values."
  {27 parse-tagged-literal
   39 parse-symbol})
