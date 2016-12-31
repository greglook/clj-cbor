(ns clj-cbor.core
  "Core CBOR library API."
  (:require
    [clj-cbor.codec :as codec]
    [clj-cbor.error :as error]
    (clj-cbor.tags
      [clojure :refer [clojure-read-handlers
                       clojure-write-handlers]]
      [content :refer [content-write-handlers
                       content-read-handlers
                       format-self-described]]
      [numbers :refer [number-read-handlers
                       number-write-handlers]]
      [time :refer [instant-read-handlers
                    epoch-time-write-handlers]]
      [text :refer [text-read-handlers
                    text-write-handlers]])
    [clojure.java.io :as io])
  (:import
    (java.io
      ByteArrayOutputStream
      DataInputStream
      DataOutputStream
      EOFException
      OutputStream)))


;; ## Codec Construction

(defn cbor-codec
  "Constructs a new CBOR codec with no configuration. Note that this does not
  include **any** read and write handlers. See the `default-codec` and the
  `default-read-handlers` and `default-write-handlers` vars.

  Arguments may be a map or a sequence of key/value pairs. Valid options are:

  - `:write-dispatch` function which is called to provide a dispatch value
    based on the data to be rendered. (default: `class`)
  - `:write-handlers` lookup function from dispatch values to handlers which
    take some data to be encoded and return a transformed version of it
    (typically a tagged value).
  - `:read-handlers` lookup function from integer tags to handlers which take
    the embedded item and return the parsed data value."
  [& opts]
  (merge
    (codec/blank-codec)
    (if (and (= 1 (count opts)) (map? (first opts)))
      (first opts)
      (apply hash-map opts))))


(def default-write-handlers
  "Map of default write handlers to use, keyed by class.

  The default choice of encoding for instants in time is the numeric epoch
  representation (tag 1)."
  (merge clojure-write-handlers
         content-write-handlers
         number-write-handlers
         epoch-time-write-handlers
         text-write-handlers))


(def default-read-handlers
  "Map of default tag handlers to use, keyed by tag.

  The default choice of representation for instants in time is
  `java.time.Instant`."
  (merge clojure-read-handlers
         content-read-handlers
         number-read-handlers
         instant-read-handlers
         text-read-handlers))


(def default-codec
  "Default CBOR codec to use when none is specified."
  (cbor-codec
    :write-handlers default-write-handlers
    :read-handlers default-read-handlers))



;; ## Encoding Functions

(defn encode
  "Encodes a value as CBOR data.

  In the full argument form, this writes a value to an output stream and
  returns the number of bytes written. If output is omitted, the function
  returns a byte array instead. Uses the `default-codec` if none is provided."
  ([value]
   (encode default-codec value))
  ([encoder value]
   (let [buffer (ByteArrayOutputStream.)]
     (encode encoder buffer value)
     (.toByteArray buffer)))
  ([encoder ^OutputStream output value]
   (let [data-output (DataOutputStream. output)]
     (codec/write-value encoder data-output value))))



;; ## Decoding Functions

(defn- maybe-read-header
  "Attempts to read a header byte from the input stream. If there is no more
  input, the `guard` value is returned."
  [^DataInputStream input guard]
  (try
    (.readUnsignedByte input)
    (catch EOFException _
      guard)))


(defn- try-read-value
  "Attemtps to read the rest of a CBOR value from the input stream. If the
  input ends during the read, the error handler is called with an
  `end-of-input` error."
  [decoder input header]
  (try
    (codec/read-value* decoder input header)
    (catch EOFException _
      (error/*handler* :clj-cbor.codec/end-of-input
        "Input data ended while parsing a CBOR value."
        {:header header}))))


(defn- read-input-value
  "Reads a CBOR value from the input stream, or returns the `guard` value if
  the input is already exhausted."
  [decoder input guard]
  (let [header (maybe-read-header input guard)]
    (if (identical? header guard)
      guard
      (try-read-value decoder input header))))


(defn decode
  "Decodes a sequence of CBOR values from the input.

  The input may be a byte array or coercible to an `input-stream`. Uses the
  `default-codec` if none is provided. This returns a lazy sequence, so take
  care that the input stream is not closed before the entries are realized."
  ([input]
   (decode default-codec input))
  ([decoder input]
   (let [eof-guard (Object.)
         data-input (DataInputStream. (io/input-stream input))
         read-data! #(read-input-value decoder data-input eof-guard)]
     (take-while
       #(not (identical? eof-guard %))
       (repeatedly read-data!)))))



;; ## Utility Functions

(defn self-describe
  "Wraps a value with a self-describing CBOR tag. This will cause the first few
  bytes of the data to be `D9D9F7`, which serves as a distinguishing header for
  format detection."
  [value]
  (format-self-described value))
