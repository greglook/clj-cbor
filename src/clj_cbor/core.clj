(ns clj-cbor.core
  "Core CBOR library API."
  (:refer-clojure :exclude [spit slurp])
  (:require
    [clj-cbor.codec :as codec]
    [clj-cbor.error :as error]
    [clj-cbor.tags.clojure :as tags.clj]
    [clj-cbor.tags.content :as tags.content]
    [clj-cbor.tags.numbers :as tags.num]
    [clj-cbor.tags.text :as tags.text]
    [clj-cbor.tags.time :as tags.time]
    [clojure.java.io :as io])
  (:import
    (java.io
      ByteArrayOutputStream
      DataInputStream
      DataOutputStream
      EOFException
      InputStream
      OutputStream)))


;; ## Codec Construction

(defn cbor-codec
  "Construct a new CBOR codec with no configuration. Note that this does not
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
  (merge tags.clj/clojure-write-handlers
         tags.content/content-write-handlers
         tags.num/number-write-handlers
         tags.time/epoch-time-write-handlers
         tags.text/text-write-handlers))


(def default-read-handlers
  "Map of default tag handlers to use, keyed by tag.

  The default choice of representation for instants in time is
  `java.time.Instant`."
  (merge tags.clj/clojure-read-handlers
         tags.content/content-read-handlers
         tags.num/number-read-handlers
         tags.time/instant-read-handlers
         tags.text/text-read-handlers))


(def default-codec
  "Default CBOR codec to use when none is specified."
  (cbor-codec
    :write-handlers default-write-handlers
    :read-handlers default-read-handlers))



;; ## Encoding Functions

(defn encode
  "Encode a single value as CBOR data.

  In the full argument form, this writes a value to an output stream and
  returns the number of bytes written. If output is omitted, the function
  returns a byte array instead. Uses the `default-codec` if none is provided."
  ([value]
   (encode default-codec value))
  ([encoder value]
   (let [buffer (ByteArrayOutputStream.)]
     (with-open [output (DataOutputStream. buffer)]
       (encode encoder output value))
     (.toByteArray buffer)))
  ([encoder ^OutputStream output value]
   (let [data-output (DataOutputStream. output)]
     (codec/write-value encoder data-output value))))


(defn encode-seq
  "Encode a sequence of values as CBOR data. This eagerly consumes the
  input sequence.

  In the full argument form, this writes a value to an output stream and
  returns the number of bytes written. If output is omitted, the function
  returns a byte array instead. Uses the `default-codec` if none is provided."
  ([values]
   (encode-seq default-codec values))
  ([encoder values]
   (let [buffer (ByteArrayOutputStream.)]
     (with-open [output (DataOutputStream. buffer)]
       (encode-seq encoder output values))
     (.toByteArray buffer)))
  ([encoder ^OutputStream output values]
   (let [data-output (DataOutputStream. output)]
     (transduce (map (partial encode encoder data-output)) + 0 values))))



;; ## Decoding Functions

(defn- data-input-stream
  "Coerce the argument to a `DataInputStream`."
  [input]
  (condp instance? input
    DataInputStream
    input

    InputStream
    (DataInputStream. input)

    (DataInputStream. (io/input-stream input))))


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


(defn decode
  "Decode a single CBOR value from the input.

  This uses the given codec or the `default-codec` if none is provided. If at
  the end of the input, this returns `eof-guard` or nil.

  The input must be an input stream or something coercible to one like a file
  or byte array. Note that coercion will produce a `BufferedInputStream` if the
  argument is not already a stream, so repeated reads will probably not behave
  as expected! If you need incremental parsing, make sure you pass in something
  that is already an `InputStream`."
  ([input]
   (decode default-codec input))
  ([decoder input]
   (decode decoder input nil))
  ([decoder input eof-guard]
   (let [input (data-input-stream input)
         header (maybe-read-header input eof-guard)]
     (if (identical? header eof-guard)
       eof-guard
       (try-read-value decoder input header)))))


(defn decode-seq
  "Decode a sequence of CBOR values from the input.

  This uses the given codec or the `default-codec` if none is provided. The
  returned sequence is lazy, so take care that the input stream is not closed
  before the entries are realized.

  The input must be an input stream or something coercible to one - see
  `decode` for usage notes."
  ([input]
   (decode-seq default-codec input))
  ([decoder input]
   (let [eof-guard (Object.)
         data-input (data-input-stream input)
         read-data! #(decode decoder data-input eof-guard)]
     (take-while
       #(not (identical? eof-guard %))
       (repeatedly read-data!)))))



;; ## Utility Functions

(defn spit
  "Opens an output stream to `f`, writes `value` to it, then closes the stream.

  Options may include `:append` to write to the end of the file instead of
  truncating."
  [f value & opts]
  (with-open [out ^OutputStream (apply io/output-stream f opts)]
    (encode default-codec out value)))


(defn spit-all
  "Opens an output stream to `f`, writes each element in `values` to it, then
  closes the stream.

  Options may include `:append` to write to the end of the file instead of
  truncating."
  [f values & opts]
  (with-open [out ^OutputStream (apply io/output-stream f opts)]
    (encode-seq default-codec out values)))


(defn slurp
  "Opens an input stream from `f`, reads the first value from it, then closes
  the stream."
  [f & opts]
  (with-open [in ^InputStream (apply io/input-stream f opts)]
    (decode default-codec in)))


(defn slurp-all
  "Opens an input stream from `f`, reads all values from it, then closes the
  stream."
  [f & opts]
  (with-open [in ^InputStream (apply io/input-stream f opts)]
    (doall (decode-seq default-codec in))))


(defn self-describe
  "Wraps a value with a self-describing CBOR tag. This will cause the first few
  bytes of the data to be `D9D9F7`, which serves as a distinguishing header for
  format detection."
  [value]
  (tags.content/format-self-described value))
