(ns clj-cbor.test-utils
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.data.core :refer [bytes?]])
  (:import
    (java.util
      Collection
      List
      Map
      Set)
    java.util.regex.Pattern
    javax.xml.bind.DatatypeConverter))


(defn bytes=
  "Compares the byte-array `value` to the sequence of `expected` byte values.
  Returns true if the array has the same length and matching byte values."
  [expected value]
  (and (bytes? value) (= (seq expected) (seq value))))



;; ## Hex Conversion Functions

(defn bin->hex
  ^String
  [^bytes value]
  (DatatypeConverter/printHexBinary value))


(defn hex->bin
  ^bytes
  [^String value]
  (DatatypeConverter/parseHexBinary value))


(defn decode-hex
  ([string]
   (decode-hex (cbor/cbor-codec) string))
  ([decoder string]
   (first (doall (cbor/decode decoder (hex->bin string))))))


(defn encoded-hex
  ([value]
   (encoded-hex (cbor/cbor-codec) value))
  ([encoder value]
   (bin->hex (cbor/encode encoder value))))



;; ## Equivalence Testing

(defmulti equivalent
  (fn [a b] (class a)))

(defmethod equivalent :default
  [a b]
  (= a b))

(defmethod equivalent Character
  [a b]
  (= (str a) (str b)))

(defmethod equivalent Pattern
  [a b]
  (= (str a) (str b)))

(defmethod equivalent Number
  [a b]
  (if (Double/isNaN a)
    (Double/isNaN b)
    (= a b)))

(defmethod equivalent List
  [a b]
  (and (= (count a) (count b))
       (every? true? (map equivalent a b))))

(defmethod equivalent Set
  [a b]
  (and (= (count a) (count b))
       (every? #(equivalent % (get b %)) a)))

(defmethod equivalent Map
  [a b]
  (and (= (count a) (count b))
       (first
         (reduce
           (fn [[decision remnant] [k v]]
             (let [[match-k match-v] (or (find remnant k)
                                         (first (filter (comp (partial equivalent k) key)
                                                        remnant)))]
               (if (some? match-k)
                 [(and decision
                       (equivalent k match-k)
                       (equivalent v match-v))
                  (dissoc remnant match-k)]
                 (reduced [false nil]))))
           [(= (count a) (count b)) b]
           a))))



;; ## Dynamic Codec

(def ^:dynamic *test-codec*
  (cbor/cbor-codec))


(defmacro with-codec
  [opts & body]
  `(binding [*test-codec* (cbor/cbor-codec ~@(flatten (seq opts)))]
     ~@body))


(defmacro check-roundtrip
  ([value hex-string]
   `(do (is (~'= ~hex-string (encoded-hex *test-codec* ~value)))
        (is (~'= ~value (decode-hex *test-codec* ~hex-string)))))
  ([compare-by value hex-string]
   `(do (is (~'= ~hex-string (encoded-hex *test-codec* ~value)))
        (is (~'= (~compare-by ~value) (~compare-by (decode-hex *test-codec* ~hex-string)))))))
