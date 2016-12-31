(ns clj-cbor.tags.clojure
  "Read and write handler support for Clojure types."
  (:require
    [clj-cbor.data.core :as data])
  (:import
    (clojure.lang
      Keyword
      Symbol
      TaggedLiteral)))


;; ## Symbols & Keywords

(def ^:const identifier-tag
  "Keywords and symbols are represented using tag 39 ('identifier') applied to
  the string version of the value. This adds three bytes to the size of the
  identifier itself for the header, tag code, and string header. Keywords are
  symbols whose first character is a colon (:).

  See: [https://github.com/lucas-clemente/cbor-specs/blob/master/id.md](https://github.com/lucas-clemente/cbor-specs/blob/master/id.md)"
  39)


(defn format-symbol
  [value]
  (data/tagged-value identifier-tag (str value)))


(defn parse-symbol
  [value]
  (when-not (string? value)
    (throw (ex-info (str "Symbols must be tagged strings, got: "
                         (class value))
                    {:value value})))
  (if (= \: (first value))
    (keyword (subs value 1))
    (symbol value)))



;; ## Tagged Literals

(def ^:const generic-object-tag
  "Tagged literals are represented using tag 27 ('generic object') applied to
  an array containing two elements. The first element is the string version of
  the EDN tag symbol and the second is the tagged literal form.

  See: [http://cbor.schmorp.de/generic-object](http://cbor.schmorp.de/generic-object)"
  27)


(defn format-tagged-literal
  [value]
  (data/tagged-value
    generic-object-tag
    [(str (:tag value)) (:form value)]))


(defn parse-tagged-literal
  [value]
  (when-not (and (sequential? value) (= 2 (count value)))
    (throw (ex-info (str "Sets must be tagged two-element arrays, got: "
                         (class value))
                    {:value value})))
  (tagged-literal (symbol (first value)) (second value)))



;; ## Codec Formatter/Handler Maps

(def clojure-write-handlers
  "Map of Clojure types to write handler functions."
  {Keyword       format-symbol
   Symbol        format-symbol
   TaggedLiteral format-tagged-literal})


(def clojure-read-handlers
  "Map of tag codes to read handlers to parse Clojure values."
  {generic-object-tag parse-tagged-literal
   identifier-tag     parse-symbol})
