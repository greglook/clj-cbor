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


(defn- read-value-stream
  "Reads values from the input in a streaming fashion, using the given reducing
  function and optional type predicate."
  [^DataInputStream input reducer valid-type? allow-indefinite]
  (loop [state (reducer)]
    (let [initial-byte (.readUnsignedByte input)]
      (if (= initial-byte data/break)
        ; Break code, finish up result.
        (reducer state)
        ; Read next value.
        (let [mtype (major-type initial-byte)
              info (additional-information initial-byte)]
          (cond
            ; Illegal element type.
            (not (valid-type? mtype))
              (*error-handler*
                ::illegal-chunk
                (str "Stream may not contain values of type " mtype))

            ; Illegal indefinite-length chunk.
            (and (= info 31) (not allow-indefinite))
              (*error-handler*
                ::definite-length-required
                "Stream chunks must have a definite length.")

            ; Reduce state with next value.
            :else
              (recur (reducer state (read-value input initial-byte)))))))))


(defn- read-integer
  "Reads an unsigned integer from the input stream."
  [^DataInputStream input info]
  (let [value (read-length input info)]
    (if (= :indefinite value)
      (*error-handler*
        ::definite-length-required
        "Encoded integers cannot have indefinite length.")
      value)))


(defn- read-byte-string
  "Reads a sequence of bytes from the input stream."
  [^DataInputStream input info]
  (let [length (read-length input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length byte strings.
      (read-value-stream
        input
        (fn build-bytes
          ([]
           (ByteArrayOutputStream.))
          ([buffer]
           (.toByteArray ^ByteArrayOutputStream buffer))
          ([buffer v]
           (.write ^ByteArrayOutputStream buffer ^bytes v)
           buffer))
        #{:byte-string}
        false)
      ; Read definite-length byte string.
      (read-bytes input length))))


(defn- read-text-string
  "Reads a sequence of bytes from the input stream."
  [^DataInputStream input info]
  (let [length (read-length input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length text strings.
      (read-value-stream
        input
        (fn build-text
          ([]
           (StringBuilder.))
          ([buffer]
           (str buffer))
          ([buffer v]
           (.append ^StringBuilder buffer ^String v)
           buffer))
        #{:text-string}
        false)
      ; Read definite-length text string.
      (String. (read-bytes input length) "UTF-8"))))


(defn- read-array
  "Reads an array of items from the input stream."
  [^DataInputStream input info]
  (let [length (read-length input info)]
    (if (= length :indefinite)
      ; Read streaming sequence of elements.
      (read-value-stream
        input
        (fn build-array
          ([] [])
          ([xs] xs)
          ([xs v] (conj xs v)))
        (constantly true)
        true)
      ; Read `length` elements.
      (->>
        (repeatedly #(read-value input (.readUnsignedByte input)))
        (take length)
        (vec)))))


(defn- read-map
  [^DataInputStream input info]
  (let [length (read-length input info)]
    (if (= length :indefinite)
      ; Read streaming sequence of key/value entries.
      (read-value-stream
        input
        (fn build-map
          ([] [{}])
          ([[m k :as state]]
           (if (= 1 (count state))
             m
             (*error-handler*
               ::missing-map-value
               (str "Streaming map did not contain a value for key: "
                    (pr-str k)))))
          ([[m k :as state] e]
           (if (= 1 (count state))
             ; Save key and wait for value.
             [m e]
             ; Add completed entry to map.
             [(assoc m k e)])))
        (constantly true)
        true)
      ; Read `length` entry pairs.
      (->>
        (repeatedly #(read-value input (.readUnsignedByte input)))
        (take (* 2 length))
        (apply hash-map)))))


(defn- read-tagged
  [^DataInputStream input info]
  ; FIXME: implement
  (throw (RuntimeException. "NYI")))


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
  [^DataInputStream input initial-byte]
  (let [mtype (major-type initial-byte)
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
  "Reads a single CBOR-encoded value from the input stream."
  [^InputStream input & {:keys [eof]}]
  (try
    (let [data-input (DataInputStream. input)
          initial-byte (.readUnsignedByte data-input)]
      (read-value data-input initial-byte))
    (catch EOFException ex
      ; TODO: use dynamic handler?
      (if (nil? eof)
        (throw ex)
        eof))))
