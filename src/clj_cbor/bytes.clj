(ns clj-cbor.bytes
  "Functions for reading, manipulating, and comparing bytes."
  (:import
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


(defn compare-bytes
  "Returns a negative number, zero, or a positive number when `x` is 'less
  than', 'equal to', or 'greater than' `y`.

  Sorting is performed on the bytes of the representation of the key data
  items without paying attention to the 3/5 bit splitting for major types.
  The sorting rules are:

  - If two keys have different lengths, the shorter one sorts earlier;
  - If two keys have the same length, the one with the lower value in
    (byte-wise) lexical order sorts earlier."
  [^bytes x ^bytes y]
  (let [xlen (alength x)
        ylen (alength y)
        get-byte (fn get-byte
                   [^bytes bs i]
                   (let [b (aget bs i)]
                     (if (neg? b)
                       (+ b 256)
                       b)))]
    (if (= xlen ylen)
      ; Same length - compare content.
      (loop [i 0]
        (if (< i xlen)
          (let [xi (get-byte x i)
                yi (get-byte y i)]
            (if (= xi yi)
              (recur (inc i))
              (compare xi yi)))
          0))
      ; Compare lengths.
      (compare xlen ylen))))
