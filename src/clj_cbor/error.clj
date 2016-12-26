(ns clj-cbor.error
  "Dynamic error handling support.")


(defn codec-exception!
  "Default behavior for codec errors."
  [error-type message data]
  (throw (ex-info message (assoc data :cbor/error error-type))))


(def ^:dynamic *handler*
  "Dynamic error handler which can be bound to a function which will be called
  with a type keyword, a message, and a map of extra data."
  codec-exception!)



;; ## Error Hierarchy

;; Encoding errors.
(derive :clj-cbor.header/negative-info-code ::encoding-error)
(derive :clj-cbor.header/overflow-info-code ::encoding-error)
(derive :clj-cbor.codec/illegal-simple-type ::encoding-error)
(derive :clj-cbor.codec/unsupported-type    ::encoding-error)

;; Decoding errors.
(derive :clj-cbor.header/reserved-info-code ::decoding-error)
(derive :clj-cbor.codec/illegal-chunk       ::decoding-error)
(derive :clj-cbor.codec/illegal-stream      ::decoding-error)
(derive :clj-cbor.codec/missing-map-value   ::decoding-error)
(derive :clj-cbor.codec/duplicate-map-key   ::decoding-error)
(derive :clj-cbor.codec/tag-handling-error  ::decoding-error)
(derive :clj-cbor.codec/unexpected-break    ::decoding-error)
