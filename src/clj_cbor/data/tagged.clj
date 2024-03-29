(ns clj-cbor.data.tagged
  "Type definition for CBOR tagged values.")


(deftype TaggedValue
  [tag value _meta]

  Object

  (toString
    [_]
    (str tag "(" value ")"))


  (equals
    [this that]
    (or (identical? this that)
        (and (instance? TaggedValue that)
             (= tag (.tag ^TaggedValue that))
             (= value (.value ^TaggedValue that)))))


  (hashCode
    [this]
    (hash [(class this) tag value]))


  clojure.lang.IObj

  (meta
    [_]
    _meta)


  (withMeta
    [_ meta-map]
    (TaggedValue. tag value meta-map)))
