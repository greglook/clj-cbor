(ns clj-cbor.core
  (:require
    (clj-cbor
      [decoder :as decoder]
      [encoder :as encoder])
    [clj-cbor.data.model :as data]
    [clojure.java.io :as io])
  (:import
    (java.io
      ByteArrayInputStream
      ByteArrayOutputStream
      DataInputStream
      DataOutputStream
      EOFException
      OutputStream)))


(defn encode
  "..."
  ([value]
   (encode (encoder/map->ValueEncoder nil) value))
  ([encoder value]
   (let [buffer (ByteArrayOutputStream.)]
     (encode encoder buffer value)
     (.toByteArray buffer)))
  ([encoder ^OutputStream output value]
   (let [data-output (DataOutputStream. output)]
     (encoder/encode-value* encoder data-output value))))


(defn decode
  "Returns a lazy sequence of values decoded from the CBOR input stream. The
  sequence will end with the end of the data."
  ([input]
   (decode (decoder/map->ValueDecoder nil) input))
  ([decoder input]
   (let [eof-guard (Object.)
         data-input (DataInputStream.
                      (if (data/bytes? input)
                        (java.io.ByteArrayInputStream. ^bytes input)
                        (io/input-stream input)))]
     (->>
       (repeatedly
         #(try
            (decoder/read-value decoder data-input)
            (catch EOFException ex
              eof-guard)))
       (take-while #(not (identical? eof-guard %)))))))
