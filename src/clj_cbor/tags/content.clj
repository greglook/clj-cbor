(ns clj-cbor.tags.content
  "Read and write handler support for content sharing and encoding hints."
  (:require
    [clj-cbor.data.core :as data]))



;; ## Self-Describe CBOR

;; In many applications, it will be clear from the context that CBOR is being
;; employed for encoding a data item. For instance, a specific protocol might
;; specify the use of CBOR, or a media type is indicated that specifies its
;; use. However, there may be applications where such context information is
;; not available, such as when CBOR data is stored in a file and disambiguating
;; metadata is not in use. Here, it may help to have some distinguishing
;; characteristics for the data itself.

(def ^:const self-describe-cbor-tag
  "Tag 55799 is defined for self-described CBOR values. It does not impart any
  special semantics on the data item that follows; that is, the semantics of a
  data item tagged with tag 55799 is exactly identical to the semantics of the
  data item itself."
  55799)


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
