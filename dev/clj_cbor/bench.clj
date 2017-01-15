(ns clj-cbor.bench
  "Benchmarking utilities for format comparisons."
  (:require
    [blocks.core :as block]
    [blocks.store.file :refer [file-block-store]]
    [cognitect.transit :as transit]
    [clj-cbor.core :as cbor]
    [clj-cbor.test-utils :as util]
    [clojure.data.fressian :as fressian]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.test.check.generators :as gen]
    [criterium.core :as crit]
    [multihash.core :as multihash]
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



;; ## Benchmarking Functions

(defn bench-codec
  "Benchmark a codec defined in `codecs` against the given `data` value."
  [codec-type data]
  (let [start (System/nanoTime)]
    (printf "Benchmarking codec %s...\n" (name codec-type))
    (flush)
    (try
      (let [{:keys [encoder decoder]} (get codecs codec-type)
            encoded (encoder data)
            decoded (decoder encoded)
            encode-stats (crit/quick-benchmark (encoder data) {})
            encode-mean (-> encode-stats :mean first (* 1000))
            decode-stats (crit/quick-benchmark (decoder encoded) {})
            decode-mean (-> decode-stats :mean first (* 1000))
            elapsed (/ (- (System/nanoTime) start) 1000000.0)]
        (printf "Finished %s in %.3f ms\n" (name codec-type) elapsed)
        (flush)
        {:encode encode-mean
         :decode decode-mean
         :roundtrip (+ encode-mean decode-mean)
         :size (count encoded)
         :equivalent? (util/equivalent data decoded)})
      (catch Exception ex
        (let [elapsed (/ (- (System/nanoTime) start) 1000000.0)]
          (printf "Benchmark data doesn't round-trip: %s\n"
                  (.getMessage ex))
          (flush)
          {:error (.getMessage ex)})))))


(defn bench-all
  "Benchmarks all available codecs on the given `data` value."
  [data]
  (into {} (map (juxt identity #(bench-codec % data))) (keys codecs)))



;; ## Tool Functions

(def ^:private size-thresholds
  (vec (take 20 (iterate #(* 2 %) 64))))


(defn- update-size-histogram
  [histogram size]
  (let [index (or (->> size-thresholds
                       (map vector (range))
                       (drop-while #(> size (second %)))
                       (ffirst))
                  (count size-thresholds))]
    (-> histogram
        (update-in [:buckets index] (fnil inc 0))
        (update :count inc)
        (update :size + size))))


(defn- into-size-histogram
  [sizes]
  (reduce update-size-histogram
          {:count 0
           :size 0}
          sizes))


(defn- print-size-histogram
  "Print out a human-consumable version of the histogram. Returns the histogram
  value."
  [histogram]
  (printf "%d objects in %d bytes\n" (:count histogram) (:size histogram))
  (doseq [[index bucket-count] (->> histogram :buckets (sort-by key))]
    (printf "%s bytes: %4d\n"
            (if-let [threshold (get size-thresholds index)]
              (format "< %4d" threshold)
              "> ...")
            bucket-count))
  (flush)
  histogram)


(defn generate-sample
  [size]
  ; TODO: review supported vs generated types
  ; byte-arrays
  ; dates (read as dates)
  ; bignums
  ; bigdecs
  ; sets (probably already generated)
  ; ratio
  ; URI
  ; regex (not as keys in maps or sets)
  (let [any-data (gen/recursive-gen gen/container-type
                                    gen/simple-type-printable)]
    (gen/generate any-data size)))


(defn- generate-sample-data
  [store n min-size max-size]
  (printf "Generating %d data samples...\n" n)
  (flush)
  (->> (repeatedly #(generate-sample (+ min-size (rand-int (- max-size min-size)))))
       (take n)
       (map #(:size (block/store! store (cbor/encode %))))
       (into-size-histogram)
       (print-size-histogram)))


(defn- report-sample-store
  [store]
  (println "Scanning stored sample data...")
  (flush)
  (->> (block/list store)
       (map :size)
       (into-size-histogram)
       (print-size-histogram)))


(def ^:private tsv-column-order
  [:size :encode :decode :equiv])


(defn- tsv-header
  []
  (->>
    (keys codecs)
    (sort)
    (mapcat (fn [codec-type]
              (map #(str (name codec-type) \/ (name %))
                   tsv-column-order)))
    (list* "block/id" "block/size")
    (str/join \tab)))


(defn- tsv-report-line
  [block-id block-size results]
  (->>
    (keys codecs)
    (sort)
    (map results)
    (mapcat
      (fn [result]
        (if (:error result)
          ["" "" "" ""]
          [(:size result)
           (format "%.3f" (* 1000 (:encode result)))
           (format "%.3f" (* 1000 (:decode result)))
           (if (:equivalent? result) 1 0)])))
    (list* block-id block-size)
    (str/join \tab)))


(defn- benched-blocks
  "Reads in the data TSV file and constructs a set of the ids of all the blocks
  which have been benchmarked."
  [data-file]
  (->>
    (str/split (slurp data-file) #"\n")
    (drop 1)
    (map #(first (str/split % #"\t")))
    (map multihash/decode)
    (set)))


(defn -main
  [& args]
  (let [store (file-block-store "bench/samples")]
    (case (first args)
      "gen"
      (let [n (Integer/parseInt (nth args 1 "100"))
            min-size (Integer/parseInt (nth args 2 "0"))
            max-size (if (< 3 (count args))
                       (Integer/parseInt (nth args 3 "200"))
                       min-size)]
        (generate-sample-data store min-size max-size))

      "stats"
      (report-sample-store store)

      "run"
      (let [[_ strategy rounds out-path] args
            data-file (io/file (or out-path "bench/data.tsv"))]
        (when-not (.exists data-file)
          (io/make-parents data-file)
          (spit data-file (str (tsv-header) "\n")))
        (case strategy
          "all"
          (let [stats (report-sample-store store)
                benched (benched-blocks data-file)]
            (doseq [[i block] (map-indexed vector (block/list store))
                    :let [hex-id (multihash/hex (:id block))]]
              (if (benched (:id block))
                (printf "Skipping block %d/%d (%.1f%%) %s (already tested)\n"
                        (inc i) (:count stats) (* 100.0 (/ i (:count stats)))
                        hex-id)
                (do
                  (printf "Testing block %d/%d (%.1f%%) %s (%d bytes)...\n"
                          (inc i) (:count stats) (* 100.0 (/ i (:count stats)))
                          hex-id (:size block))
                  (let [data (with-open [input (block/open (block/get store (:id block)))]
                               (first (cbor/decode input)))
                        result (bench-all data)]
                    (spit data-file
                          (str (tsv-report-line hex-id (:size block) result) "\n")
                          :append true))))))

          "random"
          '...

          (binding [*out* *err*]
            (println "Unknown run strategy:" strategy)
            (System/exit 1))))

      ; No args
      nil
      (binding [*out* *err*]
        (println "Usage: lein bench <gen|stats|run>")
        (System/exit 1))

      ; Unknown command.
      (binding [*out* *err*]
        (println "Unknown command:" (first args))
        (System/exit 1)))))
