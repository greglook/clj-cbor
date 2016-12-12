(ns clj-cbor.tags.time
  "Built-in tag support for the time extensions in RFC 7049. See section
  2.4.1.

  This namespace offers interop with both the older `java.util.Date` class as
  well as the newer `java.time.Instant`. Support for both timestamp-based
  tagged values and the more efficient epoch-based values is included."
  (:require
    [clj-cbor.data.model :as data])
  (:import
    java.time.Instant
    (java.util
      Date
      TimeZone)))


;; ## Formatting

(defn- date-format
  "Returns a new `SimpleDateFormat` object representing ISO-8601."
  ^java.text.SimpleDateFormat
  []
  (doto (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSS-00:00")
    (.setTimeZone (TimeZone/getTimeZone "GMT"))))


(defn format-date-string
  [^Date value]
  (data/tagged-value 0 (.format (date-format) value)))


(defn format-instant-string
  [^Instant value]
  (data/tagged-value 0 (.format (date-format) (Date/from value))))


(defn format-date-epoch
  [^Date value]
  (data/tagged-value 1 (/ (.getTime value) 1000.0)))


(defn format-instant-epoch
  [^Instant value]
  (data/tagged-value 1 (/ (.toEpochMilli value) 1000.0)))


(def date-time-string-formatters
  "Map of date-time types to render as tag 0 time strings."
  {Date format-date-string
   Instant format-instant-string})


(def date-time-epoch-formatters
  "Map of date-time types to render as tag 1 epoch offsets."
  {Date format-date-epoch
   Instant format-instant-epoch})



;; ## Parsing

(defn parse-string-date
  [tag value]
  (.parse (date-format) value))


(defn parse-string-instant
  [tag value]
  (Instant/parse value))


(defn parse-epoch-date
  [tag value]
  (Date. (long (* value 1000))))


(defn parse-epoch-instant
  [tag value]
  (Instant/ofEpochMilli (long (* value 1000))))


(def date-handlers
  "Map of tag handlers to parse date-times as `java.util.Date` values."
  {0 parse-string-date
   1 parse-epoch-date})


(def instant-handlers
  "Map of tag handlers to parse date-times as `java.time.Instant` values."
  {0 parse-string-instant
   1 parse-epoch-instant})
