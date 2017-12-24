(defproject mvxcvi/clj-cbor "0.6.0-SNAPSHOT"
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
  [[org.clojure/clojure "1.9.0"]]

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
    [[lein-cloverage "1.0.10"]]
    :dependencies
    [[org.clojure/test.check "0.9.0"]]}

   :repl
   {:source-paths ["dev"]}

   :bench
   {:source-paths ["dev"]
    :dependencies
    [[com.cognitect/transit-clj "0.8.300"]
     [com.taoensso/nippy "2.14.0"]
     [criterium "0.4.4"]
     [mvxcvi/blocks "1.0.0"]
     [org.clojure/data.fressian "0.2.1"]]}})
