(ns clj-cbor.tags.text
  "Built-in tag support for the text extensions in RFC 7049. See section
  2.4.4."
  (:require
    [clj-cbor.data.model :as data])
  (:import
    ,,,))


;; ## Formatting

(defn format-?
  [value]
  (data/tagged-value '? '???))

(def ?-formatters
  "Map of ??? types to render as tag ? values."
  {,,,})



;; ## Parsing

(defn parse-?
  [tag value]
  ,,,)


(def ?-handlers
  "Map of tag handlers to parse ? as ??? values."
  {,,,})
