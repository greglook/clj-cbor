(ns clj-cbor.test-utils
  (:require
    [clj-cbor.core :as cbor]
    [clj-cbor.error :as error]
    [clojure.string :as str]
    [clojure.test :refer [assert-expr do-report is]])
  (:import
    (java.util
      List
      Map
      Set)
    java.util.regex.Pattern))


;; ## Test Assertions

(defmethod assert-expr 'cbor-error?
  [msg [_ expected & body]]
  `(let [errors# (volatile! [])
         record-error# (fn [error-type# message# data#]
                         (let [error# {:type error-type#
                                       :message message#
                                       :data data#}]
                           (vswap! errors# conj error#)
                           (throw (ex-info "Abort CBOR codec" {:type ::interrupt}))))]
     (binding [error/*handler* record-error#]
       (try
         ~@body
         (catch clojure.lang.ExceptionInfo ex#
           (when-not (= ::interrupt (:type (ex-data ex#)))
             (throw ex#))))
       (if-let [~'error (first @errors#)]
         ~(if (keyword? expected)
            `(is (~'= ~expected (:type ~'error)) ~msg)
            `(is (~'= ~expected (select-keys ~'error ~(vec (keys expected)))) ~msg))
         (do-report
           {:type :fail
            :message ~(or msg (str "Expected error " expected " not found in handler calls"))
            :expected '~expected
            :actual nil})))))



;; ## Value Equivalence

(defn bytes=
  "Compares the byte-array `value` to the sequence of `expected` byte values.
  Returns true if the array has the same length and matching byte values."
  [expected value]
  (and (bytes? value) (= (seq expected) (seq value))))


(defmulti equivalent?
  "True if the two values are 'equivalent' after accounting for various
  idiosyncracies like Character support, NaN, and Patterns."
  (fn dispatch
    [a _b]
    (class a)))


(defmethod equivalent? :default
  [a b]
  (= a b))


(defmethod equivalent? (class (byte-array 0))
  [a b]
  (bytes= a b))


(defmethod equivalent? Character
  [a b]
  (= (str a) (str b)))


(defmethod equivalent? Pattern
  [a b]
  (= (str a) (str b)))


(defmethod equivalent? Double
  [a b]
  (if (Double/isNaN a)
    (and (number? b) (Double/isNaN b))
    (= a b)))


(defmethod equivalent? List
  [a b]
  (and (instance? List b)
       (= (count a) (count b))
       (every? true? (map equivalent? a b))))


(defmethod equivalent? Set
  [a b]
  (and (instance? Set b)
       (= (count a) (count b))
       (every? #(seq (filter (partial equivalent? %) b)) a)))


(defmethod equivalent? Map
  [a b]
  (and (instance? Map b)
       (= (count a) (count b))
       (loop [a a
              b b]
         (if-let [[k v] (first a)]
           (if-let [[match-k match-v]
                    (first (filter (comp (partial equivalent? k) key) b))]
             (if (equivalent? v match-v)
               (recur (dissoc a k) (dissoc b match-k))
               false)
             false)
           (empty? b)))))



;; ## Hex Conversion

(defn- byte->hex
  "Convert a single byte value to a two-character hex string."
  [b]
  (let [hex (Integer/toHexString
              (if (neg? b)
                (+ b 256)
                b))]
    (if (= 1 (count hex))
      (str "0" hex)
      hex)))


(defn- hex->byte
  "Convert a two-character hex string to a byte value."
  [octet]
  (let [b (Integer/parseInt octet 16)]
    (if (< 127 b)
      (- b 256)
      b)))


(defn bin->hex
  "Convert a byte array to a hex string."
  ^String
  [^bytes value]
  (str/upper-case (apply str (map byte->hex value))))


(defn hex->bin
  "Convert a hex string to a byte array."
  ^bytes
  [^String hex]
  (let [length (/ (count hex) 2)
        data (byte-array length)]
    (dotimes [i length]
      (let [octet (subs hex (* 2 i) (* 2 (inc i)))]
        (aset-byte data i (hex->byte octet))))
    data))


(defn decode-hex
  "Decode a single CBOR value from a hex string."
  ([string]
   (decode-hex (cbor/cbor-codec) string))
  ([decoder string]
   (cbor/decode decoder (hex->bin string))))


(defn decode-hex-all
  "Decode all CBOR values from a hex string."
  ([string]
   (decode-hex-all (cbor/cbor-codec) string))
  ([decoder string]
   (doall (cbor/decode-seq decoder (hex->bin string)))))


(defn encoded-hex
  "Encode the value as CBOR and convert to a hex string."
  ([value]
   (encoded-hex (cbor/cbor-codec) value))
  ([encoder value]
   (bin->hex (cbor/encode encoder value))))



;; ## Dynamic Codec

(def ^:dynamic *test-codec*
  (cbor/cbor-codec))


(defmacro with-codec
  [opts & body]
  `(binding [*test-codec* (cbor/cbor-codec ~opts)]
     ~@body))


(defmacro check-roundtrip
  ([value hex-string]
   `(let [value# ~value]
      (is (~'= ~hex-string (encoded-hex *test-codec* value#)))
      (is (~'= value# (decode-hex *test-codec* ~hex-string)))))
  ([compare-by value hex-string]
   `(let [value# ~value]
      (is (~'= ~hex-string (encoded-hex *test-codec* value#)))
      (is (~'= (~compare-by value#) (~compare-by (decode-hex *test-codec* ~hex-string)))))))
