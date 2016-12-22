(ns clj-cbor.tags.text
  "Built-in tag support for the text extensions in RFC 7049. See section
  2.4.4."
  (:require
    [clj-cbor.data.model :as data])
  (:import
    java.net.URI
    java.util.regex.Pattern))


;; ## URIs

(defn format-uri
  [^URI value]
  (data/tagged-value 32 (str value)))


(defn parse-uri
  [tag value]
  (when (not= tag 32)
    (throw (ex-info (str "URIs must be represented with tag 32, got: "
                         tag)
                    {:tag tag, :value value})))
  (when-not (string? value)
    (throw (ex-info (str "URIs must be tagged strings, got: "
                         (class value))
                    {:tag tag, :value value})))
  (URI. value))



;; ## Patterns

(defn format-pattern
  [^Pattern value]
  (data/tagged-value 35 (str value)))


(defn parse-pattern
  [tag value]
  (when (not= tag 35)
    (throw (ex-info (str "Regular expressions must be represented with tag 35, got: "
                         tag)
                    {:tag tag, :value value})))
  (when-not (string? value)
    (throw (ex-info (str "Regular expressions must be tagged strings, got: "
                         (class value))
                    {:tag tag, :value value})))
  (Pattern/compile value))


;; ## Codec Formatter/Handler Maps

(def text-formatters
  "Map of text types to formatting functions."
  {URI     format-uri
   Pattern format-pattern})


(def text-handlers
  "Map of tag handlers to parse text values."
  {32 parse-uri
   35 parse-pattern})
