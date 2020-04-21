(ns clj-cbor.data.simple
  "Type definition for CBOR simple values.")


;; ## Undefined Value

(deftype Undefined
  [_meta]

  Object

  (toString
    [this]
    "undefined")


  (equals
    [this that]
    (or (identical? this that)
        (instance? Undefined that)))


  (hashCode
    [this]
    (hash (class this)))


  clojure.lang.IObj

  (meta
    [this]
    _meta)


  (withMeta
    [this meta-map]
    (Undefined. meta-map)))



;; ## Generic Simple Value

(deftype SimpleValue
  [^long n _meta]

  Object

  (toString
    [this]
    (str "simple(" n ")"))


  (equals
    [this that]
    (or (identical? this that)
        (and (instance? SimpleValue that)
             (= n (.-n ^SimpleValue that)))))


  (hashCode
    [this]
    (hash-combine (hash (class this)) n))


  Comparable

  (compareTo
    [this that]
    (compare n (.-n ^SimpleValue that)))


  clojure.lang.IObj

  (meta
    [this]
    _meta)


  (withMeta
    [this meta-map]
    (SimpleValue. n meta-map)))
