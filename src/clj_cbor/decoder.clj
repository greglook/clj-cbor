(ns clj-cbor.decoder
  (:require
    [clj-cbor.data :as data]
    [clojure.string :as str]))


(defn major-type
  "Determines the major type keyword encoded by the initial byte."
  [initial-byte]
  (-> initial-byte
      (bit-and 0xE0)
      (bit-shift-right 5)
      (bit-and 0x07)
      (data/major-types)))


(defn special-information
  "Determines the special type from the additional information encoded by the
  initial byte."
  [initial-byte]
  (let [value (bit-and initial-byte 0x1F)]
    (case value
      20 :false
      21 :true
      22 :null
      23 :undefined
      24 :simple-value-byte
      25 :float16
      26 :float32
      27 :float64
      (28 29 30) nil
      31 :indefinite
      nil)))


(defn length-information
  "Determines the additional length information encoded by the initial byte.
  Returns a number for values 0 - 23 or a keyword designating one of the
  `additional-information-types`."
  [initial-byte]
  (let [value (bit-and initial-byte 0x1F)]
    (if (< value 24)
      value
      (case value
        24 :uint8
        25 :uint16
        26 :uint32
        27 :uint64
        (28 29 30) nil
        31 :indefinite))))


(defn decode-initial
  "Returns a vector of the major type keyword and additional information number
  encoded by the initial byte."
  [initial-byte]
  (let [mtype (major-type initial-byte)]
    [mtype (if (= mtype :special-value)
             (special-information initial-byte)
             (length-information initial-byte))]))
