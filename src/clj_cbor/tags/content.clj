(ns clj-cbor.tags.content
  "Read and write handler support for content sharing and encoding hints."
  (:require
    [clj-cbor.data.core :as data]))



;; ## Self-Describe CBOR

(def ^:const self-describe-cbor-tag 55799)


(defn format-self-described
  [value]
  (data/tagged-value self-describe-cbor-tag value))



;; ## Codec Formatter/Handler Maps

(def content-write-handlers
  "Map of misc types to write handler functions."
  {})


(def content-read-handlers
  "Map of tag codes to read handlers to parse misc values."
  {self-describe-cbor-tag identity})
