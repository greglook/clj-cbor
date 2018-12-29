(ns clj-cbor.jump
  "CBOR Jump Table Decoder

  Uses the first byte of an entry to determine how to decode. In many instances,
  the first byte is sufficient to directly encode a value, but in most cases can
  short-circuit a lot of conditional logic.

  https://tools.ietf.org/html/rfc7049#appendix-B"
  (:require
    [clj-cbor.bytes :as bytes]
    [clj-cbor.data.core :as data]
    [clj-cbor.decoder :as decoder])
  (:import java.io.DataInputStream))


(defn uint-expr
  "return expression that corresponds to decoding a jump entry value. Jump entries
   either directly encode some numbers [0, 24), or specify an int-type
   (byte, short, int, long) to read from the input. This expression will be
   inlined into the jump entry decoding method. "
  [int-type input-symb]
  (let [input-symb (vary-meta input-symb assoc :tag 'java.io.DataInputStream)]
    (cond
      (number? int-type) int-type
      (identical? int-type :byte)  `(.readUnsignedByte ~input-symb)
      (identical? int-type :short) `(.readUnsignedShort ~input-symb)
      (identical? int-type :int)   `(bit-and (.readInt ~input-symb) 0xFFFFFFFF)
      (identical? int-type :long) `(bytes/to-unsigned-long (.readLong ~input-symb)))))


(defmacro gen-entry
  "generate code for jump table entry, all non-atom entries, share similar
   structure of needing to read a uint value (with a fixed number of bytes)
   and that value along with potentially further decoding yields value "
  [int-type decoder-symb input-symb val-symb & result-expr]
  (let [val-expr (uint-expr int-type input-symb)]
    `(fn [~decoder-symb ~input-symb]
       (let [~val-symb ~val-expr]
         ~@result-expr))))

(def int-widths [:byte :short :int :long])

(defmacro gen-int-type-entries
  "generate entry implementations for all possible initial values
  (direct, or reading from input"
  [start-offset decoder-symb input-symb val-symb result-expr]
  `(list
    ~@(map-indexed
       (fn [idx int-type]
         [(+ idx start-offset)
          `(gen-entry ~int-type ~decoder-symb ~input-symb ~val-symb ~result-expr)])
       (concat (range 0 24) int-widths))))


(def ^:private jump-entries
  (concat

   ;; 0x00 through 0x17: direct value [0, 24)
   ;; 0x18 through 0x1b: read int from input
   (gen-int-type-entries 0 decoder input v v)

   ;; 0x20 through 0x37: Negative number
   ;; 0x38 through 0x3c: Read uint then negate
   (gen-int-type-entries 0x20 decoder input v (unchecked-dec (- v)))

   ;; 0x40 through 0x57: fixed-length byte strings
   ;; 0x58 through 0x5b: read length for byte strings
   (gen-int-type-entries 0x40 decoder input n (bytes/read-bytes input n))

   ;; 0x60 through 0x77: fixed width utf8 string
   ;; 0x78 through 0x7b: read utf8 string length
   (gen-int-type-entries 0x60 decoder input n
                         (String. (bytes/read-bytes input n) "UTF8"))

   ;; 0x80 through 0x97: fixed length array
   ;; 0x98 through 0x9b: read array length
   (gen-int-type-entries 0x80 decoder input n
                         (decoder/read-fixed-array n decoder input))

   ;; 0xa0 through 0xb7: fixed length map
   ;; 0xb8 through 0xbb: read map length
   (gen-int-type-entries 0xa0 decoder input n
                         (decoder/read-fixed-map n decoder input))


   ;; simple values: starting at 0xf4
   ;; constantly is slower since uses `applyTo`,
   ;; whereas we need only fixed 2-arity
   [[0xf4 (fn [_ _] false)]
    [0xf5 (fn [_ _] true)]
    [0xf6 (fn [_ _] nil)]
    [0xf7 (fn [_ _] data/undefined)]]))

(def jump-decoder-table
  (let [entries (object-array 256)]
    (doseq [[idx e] jump-entries]
      (aset entries idx e))
    entries))
