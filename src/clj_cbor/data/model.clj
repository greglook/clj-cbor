(ns clj-cbor.data.model
  "Type definitions and keyword identifiers for CBOR data types.")


(def major-types
  "Vector of major type keywords, indexed by the three-bit values 0-7. (§2.1)"
  [:unsigned-integer
   :negative-integer
   :byte-string
   :text-string
   :data-array
   :data-map
   :tagged-value
   :simple-value])


(def length-information-types
  "Map of keywords designating the types of additional information to the
  numeric value."
  {24 :uint8
   25 :uint16
   26 :uint32
   27 :uint64
   31 :indefinite})


(def simple-information-types
  "Set of keywords designating the types of additional information."
  {20 :false
   21 :true
   22 :null
   23 :undefined
   24 :simple-value-byte
   25 :float16
   26 :float32
   27 :float64
   31 :break})


(def ^:const break
  "Value of the break code."
  (short 0xFF))


(defn boolean?
  "Predicate which returns true if `x` is a boolean value."
  [x]
  (or (true? x) (false? x)))


(let [byte-array-class (class (byte-array 0))]
  (defn bytes?
    "Predicate which returns true if `x` is a byte-array."
    [x]
    (instance? byte-array-class x)))


(defn bytes=
  "Compares the byte-array `value` to the sequence of `expected` byte values.
  Returns true if the array has the same length and matching byte values."
  [expected value]
  (and (bytes? value) (= (seq expected) (seq value))))



;; ## Undefined Value

(deftype Undefined
  [_meta]

  Object

  (toString
    [this]
    "undefined")

  (equals
    [this that]
    (boolean (or (identical? this that) (instance? Undefined that))))

  (hashCode
    [this]
    (hash (class this)))


  clojure.lang.IObj

  (meta [this] _meta)

  (withMeta
    [this meta-map]
    (Undefined. meta-map)))


; Remove automatic constructor function.
(ns-unmap *ns* '->Undefined)


(def undefined
  "Base singleton undefined value."
  (Undefined. nil))


(defn undefined*
  "Constructs a new undefined value with metadata about why it has been
  constructed."
  [reason-key & {:as data}]
  (Undefined. {:cbor/cause (assoc data :type reason-key)}))



;; ## Simple Values

(deftype SimpleValue
  [^long n _meta]

  Object

  (toString
    [this]
    (str "simple(" n ")"))

  (equals
    [this that]
    (boolean (or (identical? this that)
                 (and (instance? SimpleValue that)
                      (= n (.n ^SimpleValue that))))))

  (hashCode
    [this]
    (hash [(class this) n]))


  clojure.lang.IObj

  (meta [this] _meta)

  (withMeta
    [this meta-map]
    (SimpleValue. n meta-map)))


; Remove automatic constructor function.
(ns-unmap *ns* '->SimpleValue)


(defn simple-value
  "Constructs a simple type for the given number."
  [n]
  (when (or (neg? n) (< 255 n))
    (throw (IllegalArgumentException.
             "Simple value codes must be in [0, 255].")))
  (SimpleValue. n nil))


(defn simple-value?
  "Predicate which tests whether `x` is a simple CBOR value."
  [x]
  (instance? SimpleValue x))



;; ## Tagged Values

(deftype TaggedValue
  [tag value _meta]

  Object

  (toString
    [this]
    (str tag "(" value ")"))

  (equals
    [this that]
    (boolean (or (identical? this that)
                 (and (instance? TaggedValue that)
                      (= tag (.tag ^TaggedValue that))
                      (= value (.value ^TaggedValue that))))))

  (hashCode
    [this]
    (hash [(class this) tag value]))


  clojure.lang.IObj

  (meta [this] _meta)

  (withMeta
    [this meta-map]
    (TaggedValue. tag value meta-map)))


; Remove automatic constructor function.
(ns-unmap *ns* '->TaggedValue)


(defn tagged-value
  "Constructs a tagged value."
  [tag value]
  (TaggedValue. tag value nil))


(defn tagged-value?
  "Predicate which tests whether `x` is a CBOR tagged value."
  [x]
  (instance? TaggedValue x))
