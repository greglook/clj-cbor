(ns clj-cbor.decoder
  (:require
    [clj-cbor.data :as data]
    [clj-cbor.data.float16 :as float16]
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

(defn- decode-header
  "Determines the major type keyword and additional information encoded by the
  header byte. §2.1"
  [initial-byte]
  [(-> initial-byte
       (bit-and 0xE0)
       (bit-shift-right 5)
       (bit-and 0x07)
       (data/major-types))
   (bit-and initial-byte 0x1F)])


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
        [(bit-and 0xFF (bit-shift-right value  0))
         (bit-and 0xFF (bit-shift-right value  8))
         (bit-and 0xFF (bit-shift-right value 16))
         (bit-and 0xFF (bit-shift-right value 24))
         (bit-and 0xFF (bit-shift-right value 32))
         (bit-and 0xFF (bit-shift-right value 40))
         (bit-and 0xFF (bit-shift-right value 48))
         (bit-and 0xFF (bit-shift-right value 56))]
        (byte-array)
        (java.math.BigInteger. 1)
        (bigint))
      ; Value fits in a long, return directly.
      value)))


(defn- read-int
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
          (format "Additional information int code %d is reserved."
                  info))
      31 :indefinite)))



;; ## Reader Functions

(defprotocol Decoder

  (read-value*
    [decoder input initial-byte]
    "Reads a single value from the `DataInputStream`, given the just-read
    initial byte."))


(defn read-value
  "Reads a single value from the `DataInputStream`."
  [decoder ^DataInputStream input]
  (read-value* decoder input (.readUnsignedByte input)))


(defn- read-value-stream
  "Reads values from the input in a streaming fashion, using the given reducing
  function and optional type predicate."
  [decoder ^DataInputStream input outer-type reducer valid-type? allow-indefinite]
  (loop [state (reducer)]
    (let [initial-byte (.readUnsignedByte input)]
      (if (== initial-byte data/break)
        ; Break code, finish up result.
        (reducer state)
        ; Read next value.
        (let [[mtype info] (decode-header initial-byte)]
          (cond
            ; Illegal element type.
            (not (valid-type? mtype))
              (*error-handler*
                ::illegal-chunk
                (str outer-type " stream may not contain values of type " mtype))

            ; Illegal indefinite-length chunk.
            (and (= info 31) (not allow-indefinite))
              (*error-handler*
                ::definite-length-required
                (str outer-type " stream elements must have a definite length"))

            ; Reduce state with next value.
            :else
              (recur (reducer state (read-value* decoder input initial-byte)))))))))



;; ## Major Types

(defn- read-integer
  "Reads an unsigned integer from the input stream."
  [_ ^DataInputStream input info]
  (let [value (read-int input info)]
    (if (= :indefinite value)
      (*error-handler*
        ::definite-length-required
        "Encoded integers cannot have indefinite length.")
      value)))


(defn- read-byte-string
  "Reads a sequence of bytes from the input stream."
  [decoder ^DataInputStream input info]
  (let [length (read-int input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length byte strings.
      (read-value-stream
        decoder input :byte-string
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
  [decoder ^DataInputStream input info]
  (let [length (read-int input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length text strings.
      (read-value-stream
        decoder input :text-string
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
  [decoder ^DataInputStream input info]
  (let [length (read-int input info)]
    (if (= length :indefinite)
      ; Read streaming sequence of elements.
      (read-value-stream
        decoder input :data-array
        (fn build-array
          ([] [])
          ([xs] xs)
          ([xs v] (conj xs v)))
        (constantly true)
        true)
      ; Read `length` elements.
      (->>
        (repeatedly #(read-value decoder input))
        (take length)
        (vec)))))


(defn- read-map
  [decoder ^DataInputStream input info]
  (let [length (read-int input info)]
    (if (= length :indefinite)
      ; Read streaming sequence of key/value entries.
      (read-value-stream
        decoder input :data-map
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
        (repeatedly #(read-value decoder input))
        (take (* 2 length))
        (apply hash-map)))))


(defn- read-tagged
  [decoder ^DataInputStream input info]
  ; FIXME: implement
  (throw (RuntimeException. "NYI")))


(defn- read-simple
  "Reads a simple value from the input."
  [_ ^DataInputStream input info]
  (case info
    20 false
    21 true
    22 nil
    23 data/undefined
    24 (data/simple-value (.readUnsignedByte input))
    25 (float16/from-bits (.readUnsignedShort input))
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


(defrecord ValueDecoder
  []

  Decoder

  (read-value*
    [this input initial-byte]
    (let [[mtype info] (decode-header initial-byte)]
      (case mtype
        :unsigned-integer (read-integer this input info)
        :negative-integer (- -1 (read-integer this input info))
        :byte-string      (read-byte-string this input info)
        :text-string      (read-text-string this input info)
        :data-array       (read-array this input info)
        :data-map         (read-map this input info)
        :tagged-value     (read-tagged this input info)
        :simple-value     (read-simple this input info)))))


(defn decode-value
  "Reads a single CBOR-encoded value from the input stream."
  [^InputStream input & {:keys [eof]}]
  (try
    (let [data-input (DataInputStream. input)
          decoder (map->ValueDecoder {})]
      (read-value decoder data-input))
    (catch EOFException ex
      ; TODO: use dynamic handler?
      (if (nil? eof)
        (throw ex)
        eof))))


;; Ideas:
;; - StrictDecoder
;; - AnalyzingDecoder
