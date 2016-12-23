(ns clj-cbor.error)


(defn codec-exception!
  "Default behavior for codec errors."
  [error-type message data]
  (throw (ex-info message (assoc data :cbor/error error-type))))


(def ^:dynamic *handler*
  "Dynamic error handler which can be bound to a function which will be called
  with a type keyword, a message, and a map of extra data."
  codec-exception!)


; TODO: exception hierarchy
