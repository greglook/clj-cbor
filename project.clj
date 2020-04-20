(defproject mvxcvi/clj-cbor "0.7.3-SNAPSHOT"
  :description "Concise Binary Object Representation (RFC 7049)"
  :url "https://github.com/greglook/clj-cbor"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]
  :pedantic? :abort

  :aliases
  {"coverage" ["with-profile" "+coverage" "cloverage"]
   "bench" ["with-profile" "+bench" "run" "-m" "clj-cbor.bench"]
   "bench-repl" ["with-profile" "+bench" "repl"]}

  :dependencies
  [[org.clojure/clojure "1.10.1"]]

  :test-selectors
  {:default (complement :generative)
   :generative :generative}

  :hiera
  {:cluster-depth 2
   :show-external true
   :ignore-ns #{clojure user}}

  :profiles
  {:dev
   {:plugins
    [[lein-cloverage "1.1.2"]]
    :dependencies
    [[org.clojure/test.check "1.0.0"]
     [org.clojure/tools.reader "1.3.2"]
     [org.clojure/tools.namespace "1.0.0"]]}

   :repl
   {:source-paths ["dev"]}

   :coverage
   {:plugins
    [[lein-cloverage "1.1.2"]]}

   :bench
   {:source-paths ["bench"]
    :dependencies
    [[com.clojure-goes-fast/clj-async-profiler "0.4.1"]
     [com.cognitect/transit-clj "1.0.324"]
     [com.taoensso/nippy "2.14.0"]
     [criterium "0.4.5"]
     [mvxcvi/blocks "1.1.0"]
     [org.clojure/data.fressian "1.0.0"]]}})
