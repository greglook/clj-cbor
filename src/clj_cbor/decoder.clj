(ns clj-cbor.decoder
  (:require
    [clj-cbor.data :as data]
    [clojure.string :as str])
  (:import
    (java.io
      DataInputStream
      EOFException)))


;; ## Error Handling

(defn decoder-exception!
  "Default behavior for decoding errors."
  [error-type message data]
  (throw (ex-info (str "Decoding failure: " message)
                  (assoc data :error error-type))))


(def ^:dynamic *error-handler*
  "Dynamic error handler which can be bound to a function which will be called
  with a type keyword, a message, and an optional map of extra data."
  decoder-exception!)



;; ## Initial Byte Decoding

(defn major-type
  "Determines the major type keyword encoded by the initial byte. §2.1"
  [initial-byte]
  (-> initial-byte
      (bit-and 0xE0)
      (bit-shift-right 5)
      (bit-and 0x07)
      (data/major-types)))


(defn additional-information
  "Determines the additional information encoded by the initial byte."
  [initial-byte]
  (bit-and initial-byte 0x1F))


(defn- length-info
  "Determines the additional length information encoded by the initial byte.
  Returns a number for values 0 - 23 or a keyword designating one of the
  `additional-information-types`."
  [initial-byte]
  (let [value (additional-information initial-byte)]
    (if (< value 24)
      value
      (case value
        24 :uint8
        25 :uint16
        26 :uint32
        27 :uint64
        (28 29 30) nil
        31 :indefinite))))


(defn simple-info
  "Determines the simple type from the additional information encoded by the
  initial byte of Major Type 7 `:simple-value`. (§2.3)"
  [initial-byte]
  (let [value (additional-information initial-byte)]
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
      31 :break
      nil)))


(defn- read-length
  "Reads a size integer from the initial bytes of the input stream."
  [^DataInputStream input info]
  (if (< info 24)
    ; Info codes less than 24 directly represent the number.
    info
    ; Otherwise, signify the number of bytes following.
    (case info
      24 (.readUnsignedByte input)
      25 (.readUnsignedShort input)
      26 (bit-and (.readInt input) 0xFFFF)
      27 (.readLong input)  ; FIXME: might overflow
      (28 29 30) (*error-handler*
                   ::reserved-length
                   (format "Additional information length code %d is reserved."
                           info)
                   nil)
      31 :indefinite)))



;; ## Major Type Readers

(declare read-value)


(defn- read-integer
  "Reads an unsigned integer from the input stream."
  [^DataInputStream input info]
  (let [value (read-length input info)]
    (if (= :indefinite value)
      (*error-handler*
           ::definite-length-required
           "Encoded integers cannot have indefinite length."
           nil)
      value)))


(defn- read-bytes
  "Reads a sequence of bytes from the input stream."
  [^DataInputStream input info]
  (let [length (read-length input info)]
    (if (= :indefinite length)
      ; Read sequence of definite-length byte strings. §2.2.2
      (loop [value (read-value input)]
        ; Two valid cases: definite-length byte array chunks, or break code
        ,,,)
      ; Read definite-length byte string.
      (let [buffer (byte-array length)]
        (.readFully input buffer)
        buffer))))


(defn- read-array
  "Reads an array of items from the input stream."
  [^DataInputStream input info]
  ,,,)


(defn- read-map
  [^DataInputStream input info]
  ,,,)


(defn- read-tagged
  [^DataInputStream input info]
  ,,,)


(defn- read-simple
  [^DataInputStream input info]
  ,,,)


(defn read-value
  "Reads a single CBOR value from the input stream."
  [^DataInputStream input]
  (let [initial-byte (.readUnsignedByte input)
        mtype (major-type initial-byte)
        info (additional-information initial-byte)]
    (case mtype
      :unsigned-integer (read-integer input info)
      :negative-integer (- -1 (read-integer input info))
      :byte-string      (read-bytes input info)
      :text-string      (String. (read-bytes input info) "UTF-8")
      :data-array       (read-array input info)
      :data-map         (read-map input info)
      :tagged-value     (read-tagged input info)
      :simple-value     (read-simple input info))))


(defn- decode-value
  [^DataInputStream input {:keys [eof]}]
  (try
    (read-value input)
    (catch EOFException ex
      ; TODO: use dynamic handler?
      (if (nil? eof)
        (throw ex)
        eof))))
