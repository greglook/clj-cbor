(ns clj-cbor.walk)


(defn- boolean?
  [x]
  (or (true? x) (false? x)))


(defprotocol DataWalker
  "Protocol for an data structure visitor pattern."

  (walk-nil [this])
  (walk-boolean [this x])
  (walk-string [this x])
  (walk-character [this x]) ; ?
  (walk-symbol [this x])
  (walk-keyword [this x])
  (walk-number [this x])
  (walk-seq [this x])
  (walk-vector [this x])
  (walk-set [this x])
  (walk-map [this x])
  (walk-record [this x])
  (walk-tagged [this x])
  (walk-unknown [this x]))


(defn walk
  "Visits values in data structures."
  [walker x]
  (cond
    (nil? x)     (walk-nil walker)
    (boolean? x) (walk-boolean walker x)
    (string? x)  (walk-string walker x)
    (symbol? x)  (walk-symbol walker x)
    (keyword? x) (walk-keyword walker x)
    (number? x)  (walk-number walker x)
    (seq? x)     (walk-seq walker x)
    (vector? x)  (walk-vector walker x)
    (record? x)  (walk-record walker x)
    (map? x)     (walk-map walker x)
    (set? x)     (walk-set walker x)
    (tagged-literal? x) (walk-tagged walker x)
    :else        (walk-unknown walker x)))
