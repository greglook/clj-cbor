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


(defn- read-bytes
  "Reads `length` bytes from the input stream and returns them as a byte
  array."
  ^bytes
  [^DataInputStream input length]
  (let [buffer (byte-array length)]
    (.readFully input buffer)
    buffer))


(defn- read-byte-chunks
  "Reads a sequence of byte-string chunks, followed by a break code. §2.2.2"
  [^DataInputStream input]
  (let [buffer (ByteArrayOutputStream.)]
    (loop []
      (let [initial-byte (.readUnsignedByte input)
            mtype (major-type initial-byte)
            info (additional-information initial-byte)]
        (case mtype
          :byte-string
          (let [length (read-length input info)]
            (if (= length :indefinite)
              ; Illegal indefinite-length chunk.
              (*error-handler*
                ::definite-length-required
                "Streaming byte string chunks must have a definite length."
                nil)
              ; Append byte chunk to buffer.
              (do (.write buffer (read-byte-string input length))
                  (recur))))

          :simple-value
          (if (= info 31)
            ; Break code.
            (.toByteArray buffer)
            ; Illegal simple value.
            (*error-handler*
              ::illegal-chunk
              (str "Streaming byte strings may not contain simple-value " info)
              nil))

          ; Some other illegal value.
          (*error-handler*
            ::illegal-chunk
            (str "Streaming byte strings may not contain major-type " mtype)
            nil))))))


(defn- read-byte-string
  "Reads a sequence of bytes from the input stream."
  [^DataInputStream input info]
  (let [length (read-length input info)]
    (if (= length :indefinite)
      ; Read sequence of definite-length byte strings. §2.2.2
      (read-byte-chunks input)
      ; Read definite-length byte string.
      (read-bytes input length))))


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


(defn- read-value
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


(defn decode-value
  [^InputStream input & {:keys [eof]}]
  (try
    (read-value (DataInputStream. input))
    (catch EOFException ex
      ; TODO: use dynamic handler?
      (if (nil? eof)
        (throw ex)
        eof))))
