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


(defn bench-implementation
  [codec-type encoder-fn decoder-fn data]
  (let [encoded (encoder-fn data)
        decoded (decoder-fn encoded)
        encode-stats (crit/quick-benchmark (encoder-fn data) {})
        encode-time (first (:mean encode-stats))
        decode-stats (crit/quick-benchmark (decoder-fn encoded) {})
        decode-time (first (:mean decode-stats))]
    {:codec codec-type
     :encode encode-time
     :decode decode-time
     :roundtrip (+ encode-time decode-time)
     :size (count encoded)
     :equivalent? (util/equivalent data decoded)}))


(defn bench-reader
  [data]
  (bench-implementation
    :reader
    #(.getBytes (pr-str %) "UTF-8")
    #(read-string (String. ^bytes % "UTF-8"))
    data))


(defn bench-cbor
  [data]
  (bench-implementation
    :cbor
    cbor/encode
    (comp first cbor/decode)
    data))


(defn bench-fressian
  [data]
  (bench-implementation
    :fressian
    (fn fressian-encode
      [data]
      (let [^ByteBuffer buffer (fressian/write data)
            size (.remaining buffer)
            bytes (byte-array size)]
        (.get buffer bytes 0 size)
        bytes))
    (fn fressian-decode
      [bytes]
      (fressian/read (ByteBuffer/wrap bytes)))
    data))


(defn -main
  [& args]
  ,,,)
