(defproject mvxcvi/clj-cbor "0.3.0"
  :description "Concise Binary Object Representation (RFC 7049)"
  :url "https://github.com/greglook/clj-cbor"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]
  :pedantic? :abort

  :dependencies
  [[org.clojure/clojure "1.8.0"]]

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
   {:dependencies [[org.clojure/test.check "0.9.0"]]}

   :repl
   {:source-paths ["dev"]}

   :test
   {:plugins [[lein-cloverage "1.0.9"]]}

   :doc
   {:plugins [[lein-codox "0.10.2"]
              [michaelblume/lein-marginalia "0.9.0"]]}})
