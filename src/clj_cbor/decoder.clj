(ns clj-cbor.decoder
  (:require
    [clj-cbor.data :as data]
    [clojure.string :as str])
  (:import
    (java.io
      ByteArrayOutputStream
      DataInputStream
      EOFException
      InputStream)))


;; ## Error Handling

(defn decoder-exception!
  "Default behavior for decoding errors."
  [error-type message]
  (throw (ex-info (str "Decoding failure: " message)
                  {:error error-type})))


(def ^:dynamic *error-handler*
  "Dynamic error handler which can be bound to a function which will be called
  with a type keyword, a message, and an optional map of extra data."
  decoder-exception!)



;; ## Initial Byte Decoding

(defn- major-type
  "Determines the major type keyword encoded by the initial byte. §2.1"
  [initial-byte]
  (-> initial-byte
      (bit-and 0xE0)
      (bit-shift-right 5)
      (bit-and 0x07)
      (data/major-types)))


(defn- additional-information
  "Determines the additional information encoded by the initial byte."
  [initial-byte]
  (bit-and initial-byte 0x1F))


(defn- read-bytes
  "Reads `length` bytes from the input stream and returns them as a byte
  array."
  ^bytes
  [^DataInputStream input length]
  (let [buffer (byte-array length)]
    (.readFully input buffer)
    buffer))


(defn- read-unsigned-long
  "Reads an unsigned long value from the input stream. If the value overflows
  into the negative, it is promoted to a bigint."
  [^DataInputStream input]
  (let [value (.readLong input)]
    (if (neg? value)
      ; Overflow, promote to BigInt.
      (->>
        (byte-array
          [(bit-and 0xFF (bit-shift-right value  0))
           (bit-and 0xFF (bit-shift-right value  8))
           (bit-and 0xFF (bit-shift-right value 16))
           (bit-and 0xFF (bit-shift-right value 24))
           (bit-and 0xFF (bit-shift-right value 32))
           (bit-and 0xFF (bit-shift-right value 40))
           (bit-and 0xFF (bit-shift-right value 48))
           (bit-and 0xFF (bit-shift-right value 56))])
        (byte-array)
        (java.math.BigInteger. 1)
        (bigint))
      ; Value fits in a long, return directly.
      value)))


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
      26 (bit-and (.readInt input) 0xFFFFFFFF)
      27 (read-unsigned-long input)
      (28 29 30)
        (*error-handler*
          ::reserved-length
          (format "Additional information length code %d is reserved."
                  info))
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
        "Encoded integers cannot have indefinite length.")
      value)))


(defn- read-byte-chunks
  "Reads a sequence of byte-string chunks, followed by a break code. §2.2.2"
  [^DataInputStream input]
  (let [buffer (ByteArrayOutputStream.)]
    (loop []
      (let [initial-byte (.readUnsignedByte input)]
        (if (= initial-byte data/break)
          ; Break code.
          (.toByteArray buffer)
          ; Read next byte chunk.
          (let [mtype (major-type initial-byte)
                info (additional-information initial-byte)]
            (if (= mtype :byte-string)
              ; Byte string chunk.
              (let [length (read-length input info)]
                (if (= length :indefinite)
                  ; Illegal indefinite-length chunk.
                  (*error-handler*
                    ::definite-length-required
                    "Streaming byte string chunks must have a definite length.")
                  ; Append byte chunk to buffer.
                  (do (.write buffer (read-bytes input length))
                      (recur))))
              ; Illegal chunk type.
              (*error-handler*
                ::illegal-chunk
                (str "Streaming byte strings may not contain chunks of type " mtype)))))))))


(defn- read-byte-string
  "Reads a sequence of bytes from the input stream."
  [^DataInputStream input info]
  (let [length (read-length input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length byte strings.
      (read-byte-chunks input)
      ; Read definite-length byte string.
      (read-bytes input length))))


(defn- read-text-chunks
  "Reads a sequence of text-string chunks, followed by a break code. §2.2.2"
  [^DataInputStream input]
  (let [buffer (StringBuilder.)]
    (loop []
      (let [initial-byte (.readUnsignedByte input)]
        (if (= initial-byte data/break)
          ; Break code.
          (.toString buffer)
          ; Read next byte chunk.
          (let [mtype (major-type initial-byte)
                info (additional-information initial-byte)]
            (if (= mtype :text-string)
              ; Text string chunk.
              (let [length (read-length input info)]
                (if (= length :indefinite)
                  ; Illegal indefinite-length chunk.
                  (*error-handler*
                    ::definite-length-required
                    "Streaming text string chunks must have a definite length.")
                  ; Append text chunk to buffer.
                  (do (.append buffer (String. (read-bytes input length) "UTF-8"))
                      (recur))))
              ; Illegal chunk type.
              (*error-handler*
                ::illegal-chunk
                (str "Streaming text strings may not contain chunks of type " mtype)))))))))


(defn- read-text-string
  "Reads a sequence of bytes from the input stream."
  [^DataInputStream input info]
  (let [length (read-length input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length text strings.
      (read-text-chunks input)
      ; Read definite-length text string.
      (String. (read-bytes input length) "UTF-8"))))


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


(defn- read-float16
  "Reads a half-precision IEEE floating-point number from two bytes."
  [^DataInputStream input]
  (let [combine (fn [sign exp mant]
                  (Float/intBitsToFloat
                    (bit-or (if (zero? sign) 0 Integer/MIN_VALUE)
                            (bit-shift-left (bit-or exp mant) 13))))
        value (.readUnsignedShort input)
        sign (bit-and value 0x8000)
        exp  (bit-and value 0x7c00)
        mant (bit-and value 0x03ff)]
    (cond
      ; NaN and Infinite values.
      (= exp 0x7c00)
        (combine sign 0x3fc00 mant)

      ; Normalized value.
      (not (zero? exp))
        (combine sign (+ exp 0x1c000) mant)

      ; Subnormal value.
      (not (zero? mant))
        (loop [exp 0x1c400
               mant mant]
          (if (zero? (bit-and mant 0x400))
            (recur (- exp 0x400) (bit-shift-left mant 1))
            (combine sign exp (bit-and mant 0x3ff))))

      ; +/- 0
      :else
        (combine sign exp mant))))


(defn- read-simple
  "Reads a simple value from the input."
  [^DataInputStream input info]
  (case info
    20 false
    21 true
    22 nil
    23 data/undefined
    24 (data/simple-value (.readUnsignedByte input))
    25 (read-float16 input)
    26 (.readFloat input)
    27 (.readDouble input)
    (28 29 30)
      (*error-handler*
        ::reserved-simple-type
        (format "Additional information simple-value code %d is reserved."
                info))
    31 (*error-handler*
         ::unexpected-break
         "Break encountered outside streaming context.")
    (data/simple-value info)))


(defn- read-value
  "Reads a single CBOR value from the input stream."
  [^DataInputStream input]
  (let [initial-byte (.readUnsignedByte input)
        mtype (major-type initial-byte)
        info (additional-information initial-byte)]
    (case mtype
      :unsigned-integer (read-integer input info)
      :negative-integer (- -1 (read-integer input info))
      :byte-string      (read-byte-string input info)
      :text-string      (read-text-string input info)
      :data-array       (read-array input info)
      :data-map         (read-map input info)
      :tagged-value     (read-tagged input info)
      :simple-value     (read-simple input info))))


(defn decode-value
  [^InputStream input & {:keys [eof]}]
  (try
    (read-value (DataInputStream. input))
    (catch EOFException ex
      ; TODO: use dynamic handler?
      (if (nil? eof)
        (throw ex)
        eof))))
