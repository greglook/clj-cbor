(ns clj-cbor.tags.time
  "Built-in tag support for the time extensions in RFC 7049. See section
  2.4.1.

  This namespace offers interop with both the older `java.util.Date` class as
  well as the newer `java.time.Instant`. Support for both timestamp-based
  tagged values and the more efficient epoch-based values is included."
  (:require
    [clj-cbor.data.core :as data])
  (:import
    java.time.Instant
    java.time.format.DateTimeFormatter
    java.util.Date))


(def ^:const string-time-tag
  "Tag value 0 is for date/time strings that follow the standard format
  described in RFC3339, as refined by Section 3.3 of RFC4287."
  0)


(def ^:const epoch-time-tag
  "Tag value 1 is for numerical representation of seconds relative to
  1970-01-01T00:00Z in UTC time.

  The tagged item can be a positive or negative integer (major types 0 and 1),
  or a floating-point number (major type 7 with additional information 25, 26,
  or 27). Note that the number can be negative (time before 1970-01-01T00:00Z)
  and, if a floating-point number, indicate fractional seconds."
  1)


(defn- tagged-epoch-time
  [epoch-millis]
  (data/tagged-value
    epoch-time-tag
    (if (zero? (mod epoch-millis 1000))
      (long (/ epoch-millis 1000))
      (/ epoch-millis 1000.0))))


(defn- check-epoch-form!
  [value]
  (when-not (number? value)
    (throw (ex-info (str "Tag 1 values must be tagged numbers, got: "
                         (class value))
                    {:value value}))))


(defn- check-timestamp-form!
  [value]
  (when-not (string? value)
    (throw (ex-info (str "Tag 0 values must be tagged strings, got: "
                         (class value))
                    {:value value}))))



;; ## Instants

;; These functions interoperate with the `java.time.Instant` class.

(defn format-instant-epoch
  [^Instant value]
  (tagged-epoch-time (.toEpochMilli value)))


(defn parse-epoch-instant
  [value]
  (check-epoch-form! value)
  (Instant/ofEpochMilli (long (* value 1000))))


(defn format-instant-string
  [^Instant value]
  (data/tagged-value
    string-time-tag
    (.format DateTimeFormatter/ISO_INSTANT value)))


(defn parse-string-instant
  [value]
  (check-timestamp-form! value)
  (Instant/parse value))



;; ## Dates

;; These functions interoperate with the `java.util.Date` class.

(defn format-date-epoch
  [^Date value]
  (tagged-epoch-time (.getTime value)))


(defn parse-epoch-date
  [value]
  (check-epoch-form! value)
  (Date. (long (* value 1000))))


(defn format-date-string
  [^Date value]
  (format-instant-string (.toInstant value)))


(defn parse-string-date
  [value]
  (check-timestamp-form! value)
  (Date/from (parse-string-instant value)))



;; ## Codec Maps

(def epoch-time-write-handlers
  "Map of date-time types to render as tag 1 epoch offsets."
  {Date    format-date-epoch
   Instant format-instant-epoch})


(def string-time-write-handlers
  "Map of date-time types to render as tag 0 time strings."
  {Date    format-date-string
   Instant format-instant-string})


(def instant-read-handlers
  "Map of tag handlers to parse date-times as `java.time.Instant` values."
  {string-time-tag parse-string-instant
   epoch-time-tag  parse-epoch-instant})


(def date-read-handlers
  "Map of tag handlers to parse date-times as `java.util.Date` values."
  {string-time-tag parse-string-date
   epoch-time-tag  parse-epoch-date})
