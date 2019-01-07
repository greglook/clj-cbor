(ns clj-cbor.bytes
  "Functions for reading, manipulating, and comparing bytes "
  (:import
   (clojure.lang BigInt)
   (java.io
    ByteArrayOutputStream
    DataInputStream
    DataOutputStream)))

(defn read-bytes
  "Reads `length` bytes from the input stream and returns them as a byte
  array."
  ^bytes
  [^DataInputStream input length]
  (let [buffer (byte-array length)]
    (.readFully input buffer)
    buffer))


(defn concat-bytes
  "Reducing function which builds a contiguous byte-array from a sequence of
  byte-array chunks."
  ([]
   (ByteArrayOutputStream.))
  ([buffer]
   (.toByteArray ^ByteArrayOutputStream buffer))
  ([buffer v]
   (.write ^ByteArrayOutputStream buffer ^bytes v)
   buffer))


;; ## Byte Arrays

;; Sorting is performed on the bytes of the representation of the key data
;; items without paying attention to the 3/5 bit splitting for major types.
;; The sorting rules are:
;;
;; * If two keys have different lengths, the shorter one sorts
;;   earlier;
;;
;; * If two keys have the same length, the one with the lower value
;;   in  (byte-wise) lexical order sorts earlier.

(defn compare-bytes
  "Returns a negative number, zero, or a positive number when `x` is 'less
  than', 'equal to', or 'greater than' `y`."
  [x y]
  (cond
    (< (count x) (count y)) -1
    (> (count x) (count y))  1
    :else
    (if-let [[x' y'] (some
                      (fn [[x' y' :as data]]
                        (when (not= x' y')
                          data))
                      (map vector x y))]
      (if (< x' y') -1 1)
      0)))


(def ^:private TWO_64
  (.shiftLeft (BigInteger/ONE) 64))


(defn to-unsigned-long
  "Coerce a signed long to an unsigned long. If the value overflows
   into the negative, it is promoted to a bigint (in big endian byte order
   according to CBOR spec).

  https://tools.ietf.org/html/rfc7049#section-1.2"
  [^long value]
  (if (neg? value)
    (-> value
        BigInteger/valueOf
        (.add TWO_64)
        BigInt/fromBigInteger)
    value))
