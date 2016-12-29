(ns user
  "Custom repl customization for local development."
  (:require
    [clj-cbor.core :as cbor]
    [clj-cbor.data.core :as data]
    [clojure.java.io :as io]
    [clojure.repl :refer :all]
    [clojure.string :as str]
    ;[clojure.test.check :as check]
    ;[clojure.test.check.generators :as gen]
    ;[clojure.test.check.properties :as prop]
    )
  (:import
    ,,,))


; Conditional imports from clj-stacktrace and clojure.tools.namespace:
(try (require '[clojure.stacktrace :refer [print-cause-trace]]) (catch Exception e nil))
(try (require '[clojure.tools.namespace.repl :refer [refresh]]) (catch Exception e nil))
