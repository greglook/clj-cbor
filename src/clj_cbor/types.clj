(ns clj-cbor.types)


(def major-types
  "Vector of major type keywords, indexed by the three-bit values 0-7."
  [:unsigned-integer
   :negative-integer
   :byte-string
   :text-string
   :data-array
   :data-map
   :tagged-value
   :special-value])


(defn major-type
  "Determines the major type keyword encoded by the initial byte."
  [initial-byte]
  (-> initial-byte
      (bit-and 0xE0)
      (bit-shift-right 5)
      (bit-and 0x07)
      (major-types)))


(def length-information-types
  "Map of keywords designating the types of additional information to the
  numeric value."
  {:uint8      24
   :uint16     25
   :uint32     26
   :uint64     27
   :indefinite 31})


(defn length-information
  "Determines the additional length information encoded by the initial byte.
  Returns a number for values 0 - 23 or a keyword designating one of the
  `additional-information-types`."
  [initial-byte]
  (let [value (bit-and initial-byte 0x1F)]
    (if (< value 24)
      value
      (case value
        24 :uint8
        25 :uint16
        26 :uint32
        27 :uint64
        (28 29 30) nil
        31 :indefinite))))


(def special-information-types
  "Set of keywords designating the types of additional information."
  {:false             20
   :true              21
   :null              22
   :undefined         23
   :simple-value-byte 24
   :float16           25
   :float32           26
   :float64           27
   :break             31})


(defn special-information
  "Determines the special type from the additional information encoded by the
  initial byte."
  [initial-byte]
  (let [value (bit-and initial-byte 0x1F)]
    (case value
      20 :false
      21 :true
      22 :null
      23 :undefined
      24 :simple-value-byte
      25 :float16
      26 :float32
      27 :float64
      (28 29 30) nil
      31 :indefinite
      nil)))


(deftype Undefined [])


(defn decode-initial
  "Returns a vector of the major type keyword and additional information number
  encoded by the initial byte."
  [initial-byte]
  (let [mtype (major-type initial-byte)]
    [mtype (if (= mtype :special-value)
             (special-information initial-byte)
             (length-information initial-byte))]))
