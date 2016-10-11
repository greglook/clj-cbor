(ns clj-cbor.encoder
  (:require
    [arrangement.core :as order]
    [clj-cbor.data :as data :refer [boolean? bytes?]]
    [clojure.string :as str])
  (:import
    (java.io
      DataOutputStream)))


;; ## Encoder Protocol

(defprotocol Encoder
  "Protocol for a data structure visitor pattern."

  (encode-nil [this out])
  (encode-boolean [this out x])
  (encode-bytes [this out x])
  (encode-string [this out x])
  (encode-character [this out x])
  (encode-symbol [this out x])
  (encode-keyword [this out x])
  (encode-number [this out x])
  (encode-seq [this out x])
  (encode-vector [this out x])
  (encode-set [this out x])
  (encode-map [this out x])
  (encode-record [this out x])
  (encode-tagged [this out x])
  (encode-unknown [this out x]))


(defn- encode-value*
  "Visits values in data structures."
  [encoder out x]
  (cond
    (nil? x)     (encode-nil encoder out)
    (boolean? x) (encode-boolean encoder out x)
    (bytes? x)   (encode-bytes encoder out x)
    ; TODO: check for undefined and simple special types
    (string? x)  (encode-string encoder out x)
    (symbol? x)  (encode-symbol encoder out x)
    (keyword? x) (encode-keyword encoder out x)
    (number? x)  (encode-number encoder out x)
    (seq? x)     (encode-seq encoder out x)
    (vector? x)  (encode-vector encoder out x)
    (record? x)  (encode-record encoder out x)
    (map? x)     (encode-map encoder out x)
    (set? x)     (encode-set encoder out x)
    (tagged-literal? x) (encode-tagged encoder out x)
    :else        (encode-unknown encoder out x)))



;; ## Error Handling

(defn encoder-exception!
  "Default behavior for encoding errors."
  [error-type message]
  (throw (ex-info (str "Encoding failure: " message)
                  {:error error-type})))


(def ^:dynamic *error-handler*
  "Dynamic error handler which can be bound to a function which will be called
  with a type keyword and a message."
  encoder-exception!)



;; ## Writer Functions

(defn- write-header
  "Writes a header byte for the given major-type and additional info numbers."
  [^DataOutputStream out mtype-code info]
  (.writeByte out (bit-or
                    (bit-shift-left (bit-and mtype-code 0x07) 5)
                    (bit-and info 0x1F)))
  1)


(defn- write-break
  "Writes a 'break' simple value to the output."
  [^DataOutputStream out]
  (.writeByte out data/break)
  1)


(defn- write-null
  "Writes a 'null' simple value to the output."
  [^DataOutputStream out]
  (.writeByte ^DataOutputStream out 0xF6)
  1)


(defn- write-undefined
  "Writes an 'undefined' simple value to the output."
  [^DataOutputStream out]
  (.writeByte ^DataOutputStream out 0xF7)
  1)


(defn- write-boolean
  "Writes a boolean simple value to the output."
  [^DataOutputStream out x]
  (.writeByte ^DataOutputStream out (if x 0xF5 0xF4))
  1)


(defn- write-simple
  "Writes a generic simple value for the given code and returns the number of
  bytes written. Does not handle floating-point or reserved values."
  [^DataOutputStream out ^long n]
  (cond
    (<= 0 n 23)
      (write-header out 7 n)
    (<= 32 n 255)
      (do (write-header out 7 24)
          (.writeByte out n)
          2)
    :else
      (*error-handler*
        ::illegal-simple-type
        (str "Illegal or reserved simple value: " n))))


; TODO: write-float







;; ## Encoder Implementation

(defrecord ValueEncoder
  [handlers]

  Encoder

  ; Primitive Types

  (encode-nil
    [this out]
    (.writeByte ^DataOutputStream out 2r11110110)
    1)

  (encode-boolean
    [this out x]
    (.writeByte ^DataOutputStream out (if x 0xF5 0xF4))
    1)

  (encode-bytes
    [this out x]
    ,,,)

  (encode-string
    [this out x]
    ,,,)

  (encode-symbol
    [this out x]
    ,,,)

  (encode-keyword
    [this out x]
    ,,,)

  (encode-number
    [this out x]
    ,,,)

  ; Collection Types

  (encode-seq
    [this out x]
    ,,,)

  (encode-vector
    [this out x]
    ,,,)

  (encode-set
    [this out x]
    ,,,)

  (encode-map
    [this out x]
    ,,,)

  ; Special Types

  (encode-record
    [this out x]
    ,,,)

  (encode-tagged
    [this out x]
    ,,,)

  (encode-unknown
    [this out x]
    ,,,))
