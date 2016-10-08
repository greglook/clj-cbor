(ns clj-cbor.encoder
  (:require
    [arrangement.core :as order]
    [clj-cbor.walk :as walk]
    [clojure.string :as str]))


(defrecord CBORWalker
  [handlers]

  walk/DataWalker

  ; Primitive Types

  (walk-nil
    [this]
    ,,,)

  ; Collection Types

  ; Special Types

  )


(defn cbor-walker
  "Constructs a new CBOR "
  []
  (map->CBORWalker {}))


; Remove automatic constructor function.
(ns-unmap *ns* '->CBORWalker)
