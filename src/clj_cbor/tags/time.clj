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
    java.time.format.DateTimeFormatter
    java.util.Date))


(defn- tagged-epoch-time
  [epoch-millis]
  (data/tagged-value 1
    (if (zero? (mod epoch-millis 1000))
      (long (/ epoch-millis 1000))
      (/ epoch-millis 1000.0))))


(defn- check-epoch-form!
  [tag value]
  (when (not= tag 1)
    (throw (ex-info (str "Epoch times must be represented with tag 1, got: "
                         tag)
                    {:tag tag, :value value})))
  (when-not (number? value)
    (throw (ex-info (str "Tag 1 values must be numbers, got: "
                         (class value))
                    {:tag tag, :value value}))))


(defn- check-timestamp-form!
  [tag value]
  (when (not= tag 0)
    (throw (ex-info (str "ISO timestamps must be represented with tag 0, got: "
                         tag)
                    {:tag tag, :value value})))
  (when-not (string? value)
    (throw (ex-info (str "Tag 0 values must be strings, got: "
                         (class value))
                    {:tag tag, :value value}))))



;; ## Instants

(defn format-instant-epoch
  [^Instant value]
  (tagged-epoch-time (.toEpochMilli value)))


(defn parse-epoch-instant
  [tag value]
  (check-epoch-form! tag value)
  (Instant/ofEpochMilli (long (* value 1000))))


(defn format-instant-string
  [^Instant value]
  (data/tagged-value 0
    (.format DateTimeFormatter/ISO_INSTANT value)))


(defn parse-string-instant
  [tag value]
  (check-timestamp-form! tag value)
  (Instant/parse value))



;; ## Dates

(defn format-date-epoch
  [^Date value]
  (tagged-epoch-time (.getTime value)))


(defn parse-epoch-date
  [tag value]
  (check-epoch-form! tag value)
  (Date. (long (* value 1000))))


(defn format-date-string
  [^Date value]
  (format-instant-string (.toInstant value)))


(defn parse-string-date
  [tag value]
  (check-timestamp-form! tag value)
  (Date/from (parse-string-instant tag value)))



;; ## Codec Maps

(def time-epoch-formatters
  "Map of date-time types to render as tag 1 epoch offsets."
  {Date format-date-epoch
   Instant format-instant-epoch})


(def time-string-formatters
  "Map of date-time types to render as tag 0 time strings."
  {Date format-date-string
   Instant format-instant-string})


(def instant-handlers
  "Map of tag handlers to parse date-times as `java.time.Instant` values."
  {0 parse-string-instant
   1 parse-epoch-instant})


(def date-handlers
  "Map of tag handlers to parse date-times as `java.util.Date` values."
  {0 parse-string-date
   1 parse-epoch-date})
