(ns clj-cbor.test-utils
  (:require
    [clj-cbor.core :as cbor]
    [clj-cbor.error :as error]
    [clojure.test :refer [assert-expr do-report is]])
  (:import
    javax.xml.bind.DatatypeConverter))


(defn bytes=
  "Compares the byte-array `value` to the sequence of `expected` byte values.
  Returns true if the array has the same length and matching byte values."
  [expected value]
  (and (bytes? value) (= (seq expected) (seq value))))


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



;; ## Hex Conversion

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
   (cbor/decode decoder (hex->bin string))))


(defn decode-hex-all
  ([string]
   (decode-hex-all (cbor/cbor-codec) string))
  ([decoder string]
   (doall (cbor/decode-seq decoder (hex->bin string)))))


(defn encoded-hex
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
