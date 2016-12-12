(ns clj-cbor.core
  (:require
    [clj-cbor.codec :as codec]
    [clj-cbor.data.model :as data]
    (clj-cbor.tags
      [numbers :as numbers]
      [time :as time]
      [text :as text])
    [clojure.java.io :as io])
  (:import
    (java.io
      ByteArrayInputStream
      ByteArrayOutputStream
      DataInputStream
      DataOutputStream
      EOFException
      OutputStream)))


(defn cbor-codec
  [& {:as opts}]
  (codec/map->CBORCodec
    (merge
      {:formatter-dispatch class
       :formatters (merge numbers/bignum-formatters
                          time/date-time-string-formatters)
       :tag-handlers (merge numbers/bignum-handlers
                            time/instant-handlers)}
      opts)))


(defn encode
  "..."
  ([value]
   (encode (codec/map->CBORCodec nil) value))
  ([encoder value]
   (let [buffer (ByteArrayOutputStream.)]
     (encode encoder buffer value)
     (.toByteArray buffer)))
  ([encoder ^OutputStream output value]
   (let [data-output (DataOutputStream. output)]
     (codec/write-value encoder data-output value))))


(defn decode
  "Returns a lazy sequence of values decoded from the CBOR input stream. The
  sequence will end with the end of the data."
  ([input]
   (decode (codec/map->CBORCodec nil) input))
  ([decoder input]
   (let [eof-guard (Object.)
         data-input (DataInputStream.
                      (if (data/bytes? input)
                        (java.io.ByteArrayInputStream. ^bytes input)
                        (io/input-stream input)))]
     (->>
       (repeatedly
         #(try
            (codec/read-value decoder data-input)
            (catch EOFException ex
              eof-guard)))
       (take-while #(not (identical? eof-guard %)))))))
