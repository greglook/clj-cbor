(defproject mvxcvi/clj-cbor "1.1.1"
  :description "Concise Binary Object Representation (RFC 7049)"
  :url "https://github.com/greglook/clj-cbor"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["main"]
  :pedantic? :abort

  :aliases
  {"coverage" ["with-profile" "+coverage" "cloverage"]
   "bench" ["with-profile" "+bench" "run" "-m" "clj-cbor.bench"]
   "bench-repl" ["with-profile" "+bench" "repl"]}

  :plugins
  [[lein-cloverage "1.2.2"]]

  :dependencies
  [[org.clojure/clojure "1.11.1"]]

  :test-selectors
  {:default (complement :generative)
   :generative :generative}

  :hiera
  {:cluster-depth 2
   :show-external true
   :ignore-ns #{clojure user}}

  :profiles
  {:dev
   {:dependencies
    [[org.clojure/test.check "1.1.1"]
     [org.clojure/tools.reader "1.3.6"]
     [org.clojure/tools.namespace "1.3.0"]]}

   :repl
   {:source-paths ["dev"]}

   :bench
   {:source-paths ["bench"]
    :dependencies
    [[com.clojure-goes-fast/clj-async-profiler "0.5.1"]
     [com.cognitect/transit-clj "1.0.329"]
     [com.taoensso/nippy "3.1.1"]
     [criterium "0.4.6"]
     [mvxcvi/blocks "1.1.0"]
     [org.clojure/data.fressian "1.0.0"]]}})
