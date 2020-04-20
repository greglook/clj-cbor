(ns user
  "Custom repl customization for local development."
  (:require
    [clj-cbor.core :as cbor]
    [clj-cbor.data.core :as data]
    [clj-cbor.test-utils :refer :all]
    [clojure.java.io :as io]
    [clojure.repl :refer :all]
    [clojure.stacktrace :refer [print-cause-trace]]
    [clojure.string :as str]
    [clojure.tools.namespace.repl :refer [refresh]]))


;; FIXME: failing test cases:
;; [#{##NaN}]
;; [#{\space}]
(defn round-trip-equivalent?
  "True if the given value round-trips through CBOR to an 'equivalent' value.
  Returns the decoded value on success, or throws an exception on failure."
  ([value]
   (round-trip-equivalent? cbor/default-codec value))
  ([codec value]
   (let [encoded (cbor/encode codec value)
         decoded (cbor/decode encoded)]
     (if (equivalent? value decoded)
       decoded
       (throw (ex-info "Data did not round-trip to an equivalent value"
                       {:value value
                        :encoded (bin->hex encoded)
                        :decoded decoded}))))))
