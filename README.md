clj-cbor
========

[![CircleCI](https://circleci.com/gh/greglook/clj-cbor.svg?style=shield&circle-token=21efcbc50fe431aa2efc22413ba1f4407fec6283)](https://circleci.com/gh/greglook/clj-cbor)
[![API codox](https://img.shields.io/badge/doc-API-blue.svg)](https://greglook.github.io/clj-cbor/api/)
[![marginalia docs](https://img.shields.io/badge/doc-marginalia-blue.svg)](https://greglook.github.io/clj-cbor/marginalia/uberdoc.html)

A native Clojure implementation of the [Concise Binary Object Representation](http://cbor.io/)
specification.


## Installation

Library releases are published on Clojars. To use the latest version with
Leiningen, add the following dependency to your project definition:

[![Clojars Project](http://clojars.org/mvxcvi/clj-cbor/latest-version.svg)](http://clojars.org/mvxcvi/clj-cbor)


## Usage

The simple version:

```clojure
=> (require '[clj-cbor.core :as cbor])

=> (cbor/encode [0 :foo/bar true {:x 'y} #{1/3} #"foo"])
; 0x8600D827683A666F6F2F626172F5A1D827623A78D8276179CD81D81E820103D82363666F6F

=> (cbor/decode *1)
([0 :foo/bar true {:x y} #{1/3} #"foo"])
```


## Notes

A few things to keep in mind while using the library:

- Decoding half-precision (16-bit) floating-point numbers is supported, but the
  values are promoted to single-precision (32-bit) as the JVM does not have
  native support for them. There is currently no support for writing
  half-precision floats except for special values `+Inf`, `-Inf`, and `NaN`.
- CBOR does not have a type for bare characters, so they will be converted to
  single-character strings when written.
- Keywords and Symbols are represented using tag 39 for 'identifiers'.
- Tagged literals are represented using tag 27 for 'generic objects'.
- Sets are currently represented using tag 13, which is not a part of the IANA
  registry.


## License

This is free and unencumbered software released into the public domain.
See the UNLICENSE file for more information.
