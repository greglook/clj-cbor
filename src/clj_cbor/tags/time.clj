(ns clj-cbor.tags.time
  "Built-in tag support for the time extensions in RFC 7049. See section
  2.4.1.

  This namespace offers interop with both the older `java.util.Date` class as
  well as the newer `java.time.Instant`. Support for both timestamp-based
  tagged values and the more efficient epoch-based values is included."
  (:require
    [clj-cbor.data.core :as data])
  (:import
    (java.time
      Instant
      LocalDate)
    java.time.format.DateTimeFormatter
    java.util.Date))


;; ## Instants

;; Instant values represent a specific point in time, represented in the UTC
;; timezone.
;; TODO: java.sql.Timestamp compatibility? What are the java11 module implications?

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


(defn- tagged-epoch-millis
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


;; ### java.time.Instant

(defn format-instant-epoch
  "Format a `java.time.Instant` as a tagged numeric epoch offset."
  [^Instant value]
  (tagged-epoch-millis (.toEpochMilli value)))


(defn parse-epoch-instant
  "Parse a numeric epoch offset into a `java.time.Instant` value."
  [value]
  (check-epoch-form! value)
  (Instant/ofEpochMilli (long (* value 1000))))


(defn format-instant-string
  "Format a `java.time.Instant` as a tagged timestamp string."
  [^Instant value]
  (data/tagged-value
    string-time-tag
    (.format DateTimeFormatter/ISO_INSTANT value)))


(defn parse-string-instant
  "Parse a timestamp string into a `java.time.Instant` value."
  [value]
  (check-timestamp-form! value)
  (Instant/parse value))


;; ### java.util.Date

(defn format-date-epoch
  "Format a `java.util.Date` as a tagged numeric epoch offset."
  [^Date value]
  (tagged-epoch-millis (.getTime value)))


(defn parse-epoch-date
  "Parse a numeric epoch offset into a `java.util.Date` value."
  [value]
  (check-epoch-form! value)
  (Date. (long (* value 1000))))


(defn format-date-string
  "Format a `java.util.Date` as a tagged timestamp string."
  [^Date value]
  (format-instant-string (.toInstant value)))


(defn parse-string-date
  "Parse a timestamp string into a `java.util.Date` value."
  [value]
  (check-timestamp-form! value)
  (Date/from (parse-string-instant value)))



;; ## Dates

;; A local date represents a specific calendar day, without regard to any
;; particular time-zone.
;;
;; See: https://datatracker.ietf.org/doc/draft-ietf-cbor-date-tag/
;; TODO: java.sql.Date compatibility? What are the java11 module implications?

(def ^:const string-date-tag
  "Tag value 1004 is for date strings that follow the standard format
  described in RFC3339 \"full-date\" production."
  1004)


(def ^:const epoch-date-tag
  "Tag value 100 (ASCII 'd') is for numerical representation of the epoch date.
  The tagged integer is an unsigned or negative value indicating the number of
  days since the epoch date 1970-01-01."
  100)



;; ### java.time.LocalDate

(defn format-local-date-epoch
  "Format a `java.time.LocalDate` as a tagged numeric epoch offset."
  [^LocalDate value]
  (data/tagged-value
    epoch-date-tag
    (.toEpochDay value)))


(defn parse-epoch-local-date
  "Parse a numeric epoch offset into a `java.time.LocalDate` value."
  [value]
  (when-not (integer? value)
    (throw (ex-info (str "Tag 100 values must be integers, got: "
                         (class value))
                    {:value value})))
  (LocalDate/ofEpochDay (long value)))


(defn format-local-date-string
  "Format a `java.time.LocalDate` as a tagged date string."
  [^LocalDate value]
  (data/tagged-value
    string-date-tag
    (str value)))


(defn parse-string-local-date
  "Parse a date string into a `java.time.LocalDate` value."
  [value]
  (when-not (string? value)
    (throw (ex-info (str "Tag 1004 values must be strings, got: "
                         (class value))
                    {:value value})))
  (LocalDate/parse value))



;; ## Codec Maps

;; ### Instants

(def epoch-time-write-handlers
  "Map of date-time types to render as numeric epoch offsets."
  {Date    format-date-epoch
   Instant format-instant-epoch})


(def string-time-write-handlers
  "Map of date-time types to render as time strings."
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


;; ### Dates

(def epoch-date-write-handlers
  "Map of local-date types to render as numeric epoch offsets."
  {LocalDate format-local-date-epoch})


(def string-date-write-handlers
  "Map of local-date types to render as date strings."
  {LocalDate format-local-date-string})


(def local-date-read-handlers
  "Map of tag handlers to parse local-dates as `java.time.LocalDate` values."
  {string-date-tag parse-string-local-date
   epoch-date-tag  parse-epoch-local-date})
