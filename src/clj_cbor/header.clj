(ns clj-cbor.header
  "Functions for reading and writing CBOR headers."
  (:require
    [clj-cbor.error :as error])
  (:import
    clojure.lang.BigInt
    (java.io
      DataInputStream
      DataOutputStream)))


(def major-types
  "Vector of major type keywords, indexed by the three-bit values 0-7. (§2.1)"
  [:unsigned-integer
   :negative-integer
   :byte-string
   :text-string
   :data-array
   :data-map
   :tagged-value
   :simple-value])


(def ^:private major-type-codes
  "Map of major type keywords to code values."
  (zipmap major-types (range)))



;; ## Encoding Functions

(defn write-leader
  "Writes a header byte for the given major-type and additional info numbers."
  [^DataOutputStream out mtype info]
  (let [header (-> (bit-and (major-type-codes mtype) 0x07)
                   (bit-shift-left 5)
                   (bit-or (bit-and (long info) 0x1F)))]
    (.writeByte out header)))


(defn write-byte
  "Write an unsigned byte (8-bit) value to the data output stream."
  [^DataOutputStream out i]
  (.writeByte out i))


(defn write-short
  "Write an unsigned short (16-bit) value to the data output stream."
  [^DataOutputStream out i]
  (.writeShort out i))


(defn write-int
  "Write an unsigned int (32-bit) value to the data output stream. Coerces the
  value into a signed representation before writing if necessary."
  [^DataOutputStream out i]
  (.writeInt
    out
    (if (<= i Integer/MAX_VALUE)
      i
      (+ Integer/MIN_VALUE (- (dec i) Integer/MAX_VALUE)))))


(defn write-long
  "Write a long (32-bit) value to the data output stream. Coerces the value
  into a signed representation before writing if necessary."
  [^DataOutputStream out i]
  (.writeLong
    out
    (if (<= i Long/MAX_VALUE)
      i
      (+ Long/MIN_VALUE (- (dec i) Long/MAX_VALUE)))))


(defn write
  "Writes a header byte for the given major-type, plus extra bytes to encode
  the given integer code. Always writes the smallest possible representation.
  Returns the number of bytes written."
  ^long
  [^DataOutputStream out mtype i]
  (cond
    (neg? i)
    (error/*handler*
      ::negative-info-code
      (str "Cannot write negative integer code: " i)
      {:code i})

    (<= i 23)
    (do (write-leader out mtype i)
        1)

    (<= i 0xFF)
    (do (write-leader out mtype 24)
        (write-byte out i)
        2)

    (<= i 0xFFFF)
    (do (write-leader out mtype 25)
        (write-short out i)
        3)

    (<= i 0xFFFFFFFF)
    (do (write-leader out mtype 26)
        (write-int out i)
        5)

    (<= i (* -2N Long/MIN_VALUE))
    (do (write-leader out mtype 27)
        (write-long out i)
        9)

    :else
    (error/*handler*
      ::overflow-info-code
      (str "Cannot write integer code requiring 9 bytes of space: " i)
      {:code i})))



;; ## Decoding Functions

(defn decode
  "Determines the major type keyword and additional information encoded by the
  header byte. §2.1"
  [header]
  [(-> header
       (bit-and 0xE0)
       (bit-shift-right 5)
       (bit-and 0x07)
       (major-types))
   (bit-and header 0x1F)])


(def ^:private two-64
  "Constant holding `2^64` for integer manipulation."
  (.shiftLeft BigInteger/ONE 64))


(defn read-byte
  "Read an unsigned byte (8-bit) value from the data input stream. Promotes the
  value to a long for consistency."
  [^DataInputStream in]
  (long (.readUnsignedByte in)))


(defn read-short
  "Read an unsigned short (16-bit) value from the data input stream. Promotes
  the value to a long for consistency."
  [^DataInputStream in]
  (long (.readUnsignedShort in)))


(defn read-int
  "Read an unsigned int (32-bit) value from the data input stream. Promotes the
  value to a long for consistency."
  [^DataInputStream in]
  (bit-and (long (.readInt in)) 0xFFFFFFFF))


(defn read-long
  "Read an unsigned long (64-bit) value from the data input stream. Handles
  overflowing values by promoting them to a bigint.

  https://tools.ietf.org/html/rfc7049#section-1.2"
  [^DataInputStream in]
  (let [i (.readLong in)]
    (if (neg? i)
      (-> (BigInteger/valueOf i)
          (.add two-64)
          (BigInt/fromBigInteger))
      i)))


(defn read-code
  "Reads a size value from the initial bytes of the input stream. Returns
  either a number, the keyword `:indefinite`, or calls the error handler on
  reserved info codes."
  [^DataInputStream in ^long info]
  (if (< info 24)
    ; Info codes less than 24 directly represent the number.
    info
    ; Otherwise, signify the number of bytes following.
    (case info
      24 (read-byte in)
      25 (read-short in)
      26 (read-int in)
      27 (read-long in)
      (28 29 30) (error/*handler*
                   ::reserved-info-code
                   (format "Additional information int code %d is reserved."
                           info)
                   {:info info})
      31 :indefinite)))
