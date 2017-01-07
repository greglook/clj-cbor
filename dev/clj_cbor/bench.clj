(ns clj-cbor.bench
  "Benchmarking utilities for format comparisons."
  (:require
    [cognitect.transit :as transit]
    [clj-cbor.core :as cbor]
    [clj-cbor.test-utils :as util]
    [clojure.data.fressian :as fressian]
    [clojure.edn :as edn]
    [clojure.test.check.generators :as gen]
    [criterium.core :as crit]
    [taoensso.nippy :as nippy])
  (:import
    (java.io
      ByteArrayInputStream
      ByteArrayOutputStream)
    java.nio.ByteBuffer))


(def stress-data
  "Stress-testing data, mostly borrowed from `ptaoussanis/nippy`."
  {:bytes     (byte-array [(byte 1) (byte 2) (byte 3)])
   :nil       nil
   :true      true
   :false     false
   :char      \ಬ
   :str-short "foo"
   :str-long  (apply str (range 1000))
   :kw        :keyword
   :kw-ns     ::keyword
   :sym       'foo
   :sym-ns    'foo/bar
   :regex     #"^(https?:)?//(www\?|\?)?"

   :lotsa-small-numbers  (vec (range 200))
   :lotsa-small-keywords (->> (java.util.Locale/getISOLanguages)
                              (mapv keyword))
   :lotsa-small-strings  (->> (java.util.Locale/getISOCountries)
                              (mapv #(.getDisplayCountry (java.util.Locale. "en" %))))

   :sorted-set   (sorted-set 1 2 3 4 5)
   :sorted-map   (sorted-map :b 2 :a 1 :d 4 :c 3)

   :list         (list 1 2 3 4 5 (list 6 7 8 (list 9 10)))
   :list-quoted  '(1 2 3 4 5 (6 7 8 (9 10)))
   :list-empty   (list)
   :vector       [1 2 3 4 5 [6 7 8 [9 10]]]
   :vector-empty []
   :map          {:a 1 :b 2 :c 3 :d {:e 4 :f {:g 5 :h 6 :i 7}}}
   :map-empty    {}
   :set          #{1 2 3 4 5 #{6 7 8 #{9 10}}}
   :set-empty    #{}
   :nested       [#{{1 [:a :b] 2 [:c :d] 3 [:e :f]} [] #{:a :b}}
                  #{{1 [:a :b] 2 [:c :d] 3 [:e :f]} [] #{:a :b}}
                  [1 [1 2 [1 2 3 [1 2 3 4 [1 2 3 4 5]]]]]]

   :lazy-seq       (repeatedly 1000 rand)
   :lazy-seq-empty (map identity '())

   :short        (short 42)
   :integer      (int 3)
   :long         (long 3)
   :bigint       (bigint 31415926535897932384626433832795)

   :float        (float 3.14)
   :double       (double 3.14)
   :bigdec       (bigdec 3.1415926535897932384626433832795)

   :ratio        22/7
   :uuid         (java.util.UUID/randomUUID)
   :date         (java.util.Date.)})


;; Codec Definitions

(defn fressian-encode
  [data]
  (let [^ByteBuffer buffer (fressian/write data)
        size (.remaining buffer)
        bytes (byte-array size)]
    (.get buffer bytes 0 size)
    bytes))


(defn fressian-decode
  [content]
  (fressian/read (ByteBuffer/wrap content)))


(defn transit-encode
  [type data]
  (let [out (ByteArrayOutputStream.)
        writer (transit/writer out type)]
    (transit/write writer data)
    (.toByteArray out)))


(defn transit-decode
  [type content]
  (let [in (ByteArrayInputStream. ^bytes content)
        reader (transit/reader in type)]
    (transit/read reader)))


(def codecs
  "Map of codec definitions for the benchmarking harness."
  {:reader
   {:encoder #(.getBytes (pr-str %) "UTF-8")
    :decoder #(read-string (String. ^bytes % "UTF-8"))}

   :cbor
   {:encoder cbor/encode
    :decoder (comp first cbor/decode)}

   :nippy
   {:encoder nippy/freeze
    :decoder nippy/thaw}

   :fressian
   {:encoder fressian-encode
    :decoder fressian-decode}

   :transit+json
   {:encoder (partial transit-encode :json)
    :decoder (partial transit-decode :json)}

   :transit+msgpack
   {:encoder (partial transit-encode :msgpack)
    :decoder (partial transit-decode :msgpack)}})


(defn bench-codec
  "Benchmark a codec defined in `codecs` against the given `data` value."
  [codec-type data]
  (let [{:keys [encoder decoder]} (get codecs codec-type)
        encoded (encoder data)
        decoded (decoder encoded)
        encode-stats (crit/quick-benchmark (encoder data) {})
        encode-mean (-> encode-stats :mean first (* 1000))
        decode-stats (crit/quick-benchmark (decoder encoded) {})
        decode-mean (-> decode-stats :mean first (* 1000))]
    {:codec codec-type
     :encode encode-mean
     :decode decode-mean
     :roundtrip (+ encode-mean decode-mean)
     :size (count encoded)
     :equivalent? (util/equivalent data decoded)}))


(defn bench-all
  "Benchmarks all available codecs on the given `data` value."
  [data]
  ; Check all codecs for basic round-trip first.
  (doseq [codec-type (keys codecs)]
    (try
      (let [codec (get codecs codec-type)]
        ((:decoder codec) ((:encoder codec) data)))
      (catch Exception ex
        (throw (ex-info (str "Benchmark data doesn't round-trip on codec " codec-type)
                        {:type codec-type
                         :data data
                         :error ex})))))
  ; Return sequence of results.
  (mapv
    (fn [codec-type]
      (printf "Benchmarking codec %s...\n" (name codec-type))
      (flush)
      (let [start (System/nanoTime)
            result (bench-codec codec-type data)
            elapsed (/ (- (System/nanoTime) start) 1000000.0)]
        (printf "Finished %s in %.3f ms\n" (name codec-type) elapsed)
        (flush)
        result))
    (keys codecs)))


(defn -main
  [& args]
  ,,,)
