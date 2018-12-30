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
    [clojure.set :as set]
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
   {:dependency 'org.clojure/clojure
    :version "1.8.0"
    :encoder #(.getBytes (pr-str %) "UTF-8")
    :decoder #(read-string (String. ^bytes % "UTF-8"))}

   :cbor
   {:dependency 'mvxcvi/clj-cbor
    :version "0.3.0"
    :encoder cbor/encode
    :decoder (comp first cbor/decode)}

   :cbor-jump
   (let [decoder (cbor/with-jump-table (cbor/cbor-codec))]
     {:dependency 'mvxcvi/clj-cbor
      :version "0.3.0"
      :encoder cbor/encode
      :decoder (comp first (partial cbor/decode decoder))})


   :nippy
   {:dependency 'com.taoensso/nippy
    :version "2.12.2"
    :encoder nippy/freeze
    :decoder nippy/thaw}

   :fressian
   {:dependency 'org.clojure/data.fressian
    :version "0.2.1"
    :encoder fressian-encode
    :decoder fressian-decode}

   :transit+json
   {:dependency 'com.cognitect/transit-clj
    :version "0.8.297"
    :encoder (partial transit-encode :json)
    :decoder (partial transit-decode :json)}

   :transit+msgpack
   {:dependency 'com.cognitect/transit-clj
    :version "0.8.297"
    :encoder (partial transit-encode :msgpack)
    :decoder (partial transit-decode :msgpack)}})



;; ## Benchmarking Functions

(defn bench-codec
  "Benchmark a codec defined in `codecs` against the given `data` value."
  [codec-type data]
  (let [start (System/nanoTime)
        {:keys [version encoder decoder]} (get codecs codec-type)]
    (printf "Benchmarking codec %s...\n" (name codec-type))
    (flush)
    (try
      (let [encoded (encoder data)
            decoded (decoder encoded)
            encode-stats (crit/quick-benchmark (encoder data) {})
            encode-mean (-> encode-stats :mean first (* 1000))
            decode-stats (crit/quick-benchmark (decoder encoded) {})
            decode-mean (-> decode-stats :mean first (* 1000))
            elapsed (/ (- (System/nanoTime) start) 1000000.0)]
        (printf "Finished %s in %.3f ms (mean encode: %.3fµs, decode: %.3fµs)\n"
                (name codec-type) elapsed (* 1000 encode-mean) (* 1000 decode-mean))
        (flush)
        {:codec codec-type
         :version version
         :size (count encoded)
         :encode encode-mean
         :decode decode-mean})
      (catch Exception ex
        (let [elapsed (/ (- (System/nanoTime) start) 1000000.0)]
          (printf "Benchmark data doesn't round-trip: %s\n"
                  (.getMessage ex))
          (flush)
          {:error (.getMessage ex)
           :codec codec-type
           :version version})))))



;; ## Size Histograms

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



(defn- report-sample-store
  [store]
  (println "Scanning stored sample data...")
  (flush)
  (->> (block/list store)
       (map :size)
       (into-size-histogram)
       (print-size-histogram)))



;; ## Data Generation

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



;; ## TSV Output

(def ^:private tsv-columns
  [:block-id :block-size :codec :version :size :encode :decode])


(defn- tsv-header
  []
  (str/join \tab (map name tsv-columns)))


(defn- tsv-report-line
  [block-id block-size result]
  (->>
    (if (contains? result :error)
      ["!" "!" "!"]
      [(:size result)
       (format "%.3f" (* 1000 (:encode result)))
       (format "%.3f" (* 1000 (:decode result)))])
    (list* (multihash/hex block-id)
           block-size
           (subs (str (:codec result)) 1)
           (:version result))
    (str/join \tab)))


(defn- benched-blocks
  "Reads in the data TSV file and constructs a set of the ids of all the blocks
  which have been benchmarked already. Returns a map from codec/version pairs
  to sets of benchmarked block ids."
  [data-file]
  (->>
    (slurp data-file)
    (str/split-lines)
    (drop 1)
    (map #(str/split % #"\t"))
    (reduce
      (fn [benched [block-id _ codec version]]
        (update benched
                [(keyword codec) version]
                (fnil conj #{})
                (multihash/decode block-id)))
      {})))



;; ## Entry Point

(defn -main
  [& args]
  (let [store (file-block-store "bench/samples")
        data-file (io/file "bench/data.tsv")]
    (when-not (.exists data-file)
      (io/make-parents data-file)
      (spit data-file (str (tsv-header) "\n")))
    (case (first args)
      "stats"
      (report-sample-store store)

      "gen" ; n min-size max-size
      (let [n (Integer/parseInt (nth args 1 "100"))
            min-size (Integer/parseInt (nth args 2 "0"))
            max-size (Integer/parseInt (nth args 3 "200"))]
        (generate-sample-data store n min-size max-size))

      "run" ; codec...
      (let [data-file (io/file "bench/data.tsv")
            targets (if-let [names (next args)]
                      (map keyword names)
                      (keys codecs))
            benched (->> targets
                         (map #(vector % (get-in codecs [% :version])))
                         (map (juxt first (benched-blocks data-file)))
                         (into {}))
            already-benched? (or (apply set/intersection (vals benched)) #{})
            blocks (->> (block/list store)
                        (remove (comp already-benched? :id))
                        (shuffle)
                        (vec))]
        (printf "Benchmarking codecs %s against %d blocks\n"
                (str/join ", " targets) (count blocks))
        (doseq [[i block] (map-indexed vector blocks)]
          (printf "Testing block %d/%d (%.1f%%) %s (%d bytes)\n"
                  (inc i) (count blocks) (* 100.0 (/ i (count blocks)))
                  (multihash/base58 (:id block)) (:size block))
          (let [test-data (with-open [input (block/open (block/get store (:id block)))]
                            (first (cbor/decode input)))]
            (doseq [codec-type targets]
              (if (contains? (get benched codec-type) (:id block))
                (printf "Skipping %s (already tested)\n" codec-type)
                (let [result (bench-codec codec-type test-data)
                      out-line (tsv-report-line (:id block) (:size block) result)]
                  (spit data-file (str out-line "\n") :append true)))))))

      ; No args
      nil
      (binding [*out* *err*]
        (println "Usage: lein bench stats")
        (println "       lein bench gen [n] [min-size] [max-size]")
        (println "       lein bench run [codec ...]")
        (System/exit 1))

      ; Unknown command.
      (binding [*out* *err*]
        (println "Unknown command:" (first args))
        (System/exit 1)))))
