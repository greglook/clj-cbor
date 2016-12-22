(ns clj-cbor.tags.text-test
  (:require
    [clojure.test :refer :all]
    [clj-cbor.core :as cbor]
    [clj-cbor.tags.text :refer :all]
    [clj-cbor.test-utils :refer :all])
  (:import
    java.net.URI))


(deftest uri-coding
  (with-codec {:formatters text-formatters
               :tag-handlers text-handlers}
    (check-roundtrip (URI. "http://www.example.com") "D82076687474703A2F2F7777772E6578616D706C652E636F6D")))


(deftest pattern-coding
  (with-codec {:formatters text-formatters
               :tag-handlers text-handlers}
    (check-roundtrip str #"abc123" "D82366616263313233")))
