(ns clj-cbor.decoder
  "Functions related to decoding, including `Decoder` protocol. This is
  separated out from `clj-cbor.codec` in order to allow re-use in the jump
  decoder table (`clj-cbor.jump`). The streaming (or chunking) logic is left
  in `clj-cbor.codec` since it's not re-used."
  (:require [clj-cbor.error :as error])
  (:import
   (java.io DataInputStream)))

(defprotocol Decoder
  "A _decoder_ is a process that reads a CBOR data item and makes it available
  to an application.

  Formally speaking, a decoder contains a parser to break up the input using
  the syntax rules of CBOR, as well as a semantic processor to prepare the data
  in a form suitable to the application."

  (read-value*
    [decoder input header]
    "Reads a single value from the `DataInputStream`, given the just-read
    initial byte."))

(defn read-value
  "Reads a single value from the `DataInputStream`."
  [decoder ^DataInputStream input]
  (read-value* decoder input (.readUnsignedByte input)))

(defn read-fixed-array [^long n decoder input]
  (loop [result (transient []) idx 0]
    (if (= idx n)
      (persistent! result)
      (recur (conj! result (read-value decoder input))
             (unchecked-inc idx)))))

(defn read-fixed-map [^long n decoder input]
  (loop [result (transient {}) idx 0]
    (if (= idx n)
      (persistent! result)
      (let [k (read-value decoder input)]
        (if (contains? result k)
          (error/*handler* :clj-cbor.codec/duplicate-map-key
           (str "Encoded map contains duplicate key: " (pr-str k))
           {:map (persistent! result), :key k})
          (recur (assoc! result k (read-value decoder input))
                 (unchecked-inc idx)))))))
