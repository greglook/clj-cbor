(defproject mvxcvi/clj-cbor "0.1.0-SNAPSHOT"
  :description "Clojure library for Concise Binary Object Representation."
  :url "https://github.com/greglook/clj-cbor"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]
  :pedantic? :abort

  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [mvxcvi/arrangement "1.1.0"]]

  :hiera
  {:cluster-depth 1
   :show-external true
   :ignore-ns #{clojure user}}

  :codox
  {:metadata {:doc/format :markdown}
   :source-uri "https://github.com/greglook/clj-cbor/blob/master/{filepath}#L{line}"
   :doc-paths ["doc/extra"]
   :output-path "doc/api"})
