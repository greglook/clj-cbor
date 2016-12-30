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

(def ^:const uri-tag 32)


(defn format-uri
  [^URI value]
  (data/tagged-value uri-tag (str value)))


(defn parse-uri
  [value]
  (when-not (string? value)
    (throw (ex-info (str "URIs must be tagged strings, got: "
                         (class value))
                    {:value value})))
  (URI. value))



;; ## Patterns

(def ^:const pattern-tag 35)


(defn format-pattern
  [^Pattern value]
  (data/tagged-value pattern-tag (str value)))


(defn parse-pattern
  [value]
  (when-not (string? value)
    (throw (ex-info (str "Regular expressions must be tagged strings, got: "
                         (class value))
                    {:value value})))
  (Pattern/compile value))



;; ## UUIDs

;; tag 37
;; https://github.com/lucas-clemente/cbor-specs/blob/master/uuid.md

(def ^:const uuid-tag 37)


(defn format-uuid
  [^UUID value]
  (let [data (ByteBuffer/allocate 16)]
    (.putLong data (.getMostSignificantBits value))
    (.putLong data (.getLeastSignificantBits value))
    (data/tagged-value uuid-tag (.array data))))


(defn parse-uuid
  [value]
  (when-not (data/bytes? value)
    (throw (ex-info (str "UUIDs must be tagged byte strings, got: "
                         (class value))
                    {:value value})))
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
  {uri-tag     parse-uri
   pattern-tag parse-pattern
   uuid-tag    parse-uuid})
