(ns clj-cbor.tags.text
  "Built-in tag support for the text extensions in RFC 7049. See section
  2.4.4."
  (:require
    [clj-cbor.data.core :as data])
  (:import
    java.net.URI
    java.nio.ByteBuffer
    java.util.UUID
    java.util.regex.Pattern))


;; ## URIs

(defn format-uri
  [^URI value]
  (data/tagged-value 32 (str value)))


(defn parse-uri
  [tag value]
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
  (when-not (string? value)
    (throw (ex-info (str "Regular expressions must be tagged strings, got: "
                         (class value))
                    {:tag tag, :value value})))
  (Pattern/compile value))



;; ## UUIDs

; tag 37
; https://github.com/lucas-clemente/cbor-specs/blob/master/uuid.md

(defn format-uuid
  [^UUID value]
  (let [data (ByteBuffer/allocate 16)]
    (.putLong data (.getMostSignificantBits value))
    (.putLong data (.getLeastSignificantBits value))
    (data/tagged-value 37 (.array data))))


(defn parse-uuid
  [tag value]
  (when-not (data/bytes? value)
    (throw (ex-info (str "UUIDs must be tagged byte strings, got: "
                         (class value))
                    {:tag tag, :value value})))
  (let [data (ByteBuffer/wrap value)]
    (UUID. (.getLong data) (.getLong data))))



;; ## Codec Formatter/Handler Maps

(def text-write-handlers
  "Map of text types to formatting functions."
  {URI     format-uri
   UUID    format-uuid
   Pattern format-pattern})


(def text-read-handlers
  "Map of tag handlers to parse text values."
  {32 parse-uri
   35 parse-pattern
   37 parse-uuid})
