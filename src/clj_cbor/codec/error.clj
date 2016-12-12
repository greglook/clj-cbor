(ns clj-cbor.codec.error)


(defn codec-exception!
  "Default behavior for codec errors."
  ([error-type message]
   (codec-exception! error-type message nil))
  ([error-type message data]
   (throw (ex-info message (assoc data :cbor/error error-type)))))


(def ^:dynamic *handler*
  "Dynamic error handler which can be bound to a function which will be called
  with a type keyword and a message, and optionally a map of extra data. Handler
  functions should accept 2 and 3-arity calls."
  codec-exception!)
