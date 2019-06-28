(defproject mvxcvi/clj-cbor "0.7.3-SNAPSHOT"
  :description "Concise Binary Object Representation (RFC 7049)"
  :url "https://github.com/greglook/clj-cbor"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]
  :pedantic? :abort

  :aliases
  {"bench" ["with-profile" "+bench" "run" "-m" "clj-cbor.bench"]
   "bench-repl" ["with-profile" "+bench" "repl"]}

  :dependencies
  [[org.clojure/clojure "1.10.0"]]

  :test-selectors
  {:default (complement :generative)
   :generative :generative}

  :hiera
  {:cluster-depth 2
   :show-external true
   :ignore-ns #{clojure user}}

  :codox
  {:metadata {:doc/format :markdown}
   :source-uri "https://github.com/greglook/clj-cbor/blob/master/{filepath}#L{line}"
   :output-path "target/doc/api"}

  :profiles
  {:dev
   {:plugins
    [[lein-cloverage "1.1.0"]]
    :dependencies
    [[org.clojure/test.check "0.9.0"]
     [org.clojure/tools.namespace "0.2.11"]]}

   :repl
   {:source-paths ["dev"]}

   :bench
   {:source-paths ["dev"]
    :dependencies
    [[com.clojure-goes-fast/clj-async-profiler "0.3.0"]
     [com.cognitect/transit-clj "0.8.313"]
     [com.taoensso/nippy "2.14.0"]
     [criterium "0.4.4"]
     [mvxcvi/blocks "1.1.0"]
     [org.clojure/data.fressian "0.2.1"]]}})
