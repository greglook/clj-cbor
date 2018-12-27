(ns clj-cbor.jump
  "CBOR Jump Table Decoder

  Uses the first byte of an entry to determine how to decode. In many instances,
  the first byte is sufficient to directly encode a value, but in most cases can
  short-circuit logic and conditionals compared to

  https://tools.ietf.org/html/rfc7049#appendix-B"
  (:require
   [alphabase.bytes :as b]
   [clj-cbor.header :as header]
   [clj-cbor.codec :as codec])
  (:import java.io.DataInputStream))

(defprotocol JumpTableEntry
  (decode-entry [this decoder ^DataInputStream input]
    "Decode value from input data, closing over
     decoding context from header byte"))

(defn- uint-expr
  "return expression that corresponds to decoding a jump entry value. Jump entries
   either directly encode some numbers [0, 24), or specify an int-type
   (byte, short, int, long) to read from the input. This expression will be
   inlined into the jump entry decoding method. "
  [int-type input-symb]
  (let [input-symb (vary-meta input-symb assoc :tag java.io.DataInputStream)]
    (cond
      (number? int-type) int-type
      (identical? int-type :byte)  `(.readUnsignedByte ~input-symb)
      (identical? int-type :short) `(.readUnsignedShort ~input-symb)
      (identical? int-type :int)   `(bit-and (.readInt ~input-symb) 0xFFFFFFFF)
      (identical? int-type :long) `(header/read-unsigned-long ~input-symb))))


(defmacro gen-entry
  "generate code for jump table entry, all non-atom entries, share similar
   structure of needing to read a uint value (with a fixed number of bytes)
   and that value along with potentially further decoding yields value "
  [int-type decoder-symb input-symb val-symb & result-expr]
  (let [val-expr (uint-expr int-type input-symb)]
    `(reify JumpTableEntry
       (decode-entry [_ ~decoder-symb ~input-symb]
         (let [~val-symb ~val-expr]
           ~@result-expr)))))

(def int-widths [:byte :short :int :long])

(defmacro gen-int-type-entries
  "generate entry implementations for all possible initial values
  (direct, or reading from input"
  [decoder-symb input-symb val-symb result-expr]
  `(list
    ~@(for [x (concat (range 0 24) int-widths)]
        `(gen-entry ~x ~decoder-symb ~input-symb ~val-symb ~result-expr))))

(defrecord DirectValue [val]
  JumpTableEntry
  (decode-entry [_ _ _] val))

(defn- build-jump-entries []
  (concat

   ;; 0x00 through 0x17: direct value [0, 24)
   ;; 0x18 through 0x1b: read int from input
   (map-indexed
    (fn [idx entry] [idx entry])
    (gen-int-type-entries decoder input v v))

   ;; 0x20 through 0x37: Negative number
   ;; 0x38 through 0x3c: Read uint then negate
   (map-indexed
    (fn [idx entry]
      [(+ idx 0x20) entry])
    (gen-int-type-entries decoder input v (unchecked-dec (- v))))

   ;; 0x40 through 0x57: fixed-length byte strings
   ;; 0x58 through 0x5b: read length for byte strings
   (map-indexed
    (fn [idx entry]
      [(+ idx 0x40) entry])
    (gen-int-type-entries decoder input n (codec/read-bytes input n)))


   ;; 0x60 through 0x77: fixed width utf8 string
   ;; 0x78 through 0x7b: read utf8 string length
   (map-indexed
    (fn [idx entry]
      [(+ idx 0x60) entry])
    (gen-int-type-entries decoder input n (String. (codec/read-bytes input n) "UTF8")))

   ;; 0x80 through 0x97: fixed length array
   ;; 0x98 through 0x9b: read array length
   (map-indexed
    (fn [idx entry]
      [(+ idx 0x80) entry])
    (gen-int-type-entries decoder input n
                          (vec (repeatedly n #(codec/read-value decoder input)))))

   ;; 0xa0 through 0xb7: fixed length map
   ;; 0xb8 through 0xbb: read map length
   (map-indexed
    (fn [idx entry]
      [(+ idx 0xa0) entry])
    (gen-int-type-entries decoder input n
                          (->> #(codec/read-value decoder input)
                           (repeatedly (* 2 n))
                           (transduce identity codec/build-map))))


   ;; simple values: starting at 0xf4
   [[0xf4 (->DirectValue false)]
    [0xf5 (->DirectValue true)]
    [0xf6 (->DirectValue nil)]]))

(defrecord JumpTableDecoder
  [^objects jump-table base-decoder]

  codec/Encoder
  (write-value [this out x]
    (codec/write-value base-decoder out x))

  codec/Decoder
  (read-value* [this input header]
    (if-let [entry (aget jump-table (int header))]
      (decode-entry entry this input)
      (codec/read-value* base-decoder input header))))


(defn with-jump-table
  "Wrap a decoder with a jump table. When possible, this decoder will fallback
   to the same implementation as `base-decoder` if there is no jump entry for
   a header byte, or when decoding a structured object (e.g, array or map)"
  [base-decoder]
  (let [entries (object-array 256)]
    (doseq [[idx entry] (build-jump-entries)]
      (aset entries (int idx) entry))
    (->JumpTableDecoder entries base-decoder)))
