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

The `clj-cbor.core` namespace contains the high-level encoding and decoding
functions. The simplest way to use this library is to require it and call them
diretly with the data:

```clojure
=> (require '[clj-cbor.core :as cbor])

=> (cbor/encode [0 :foo/bar true {:x 'y} #{1/3} #"foo"])
; 0x8600D827683A666F6F2F626172F5A1D827623A78D8276179CD81D81E820103D82363666F6F

=> (cbor/decode *1)
([0 :foo/bar true {:x y} #{1/3} #"foo"])
```

With no extra arguments, `encode` and `decode` will make use of the
`default-codec`, which comes loaded with read and write handler support for many
Java and Clojure types (see the [type extension](#type-extension) section
below). Both functions accept an additional argument to specify the codec,
should different behavior be desired.

```clojure
=> (def codec (cbor/cbor-codec :canonical true))

=> (cbor/encode codec {:foo "bar", :baz 123})
; 0xA2D827643A666F6F63626172D827643A62617A187B

=> (cbor/decode codec *1)
({:foo "bar", :baz 123})
```

There is also an asymmetry between the functions - `encode` returns the encoded
data as a byte array, while `decode` returns a _sequence_ of values read from
the input. This behavior becomes more useful in streaming contexts, where
multiple items may present in the input stream. The full form of `encode` takes
three arguments - the codec, the output stream to write to, and the value to
encode.

```clojure
=> (def out (java.io.ByteArrayOutputStream.))

=> (cbor/encode codec out :a)
5

=> (cbor/encode codec out 123)
2

=> (cbor/encode codec out true)
1

=> (cbor/encode codec out "foo")
4

=> (.toByteArray out))
; 0xD827623A61187BF563666F6F

=> (with-open [input (java.io.ByteArrayInputStream. *1)]
     (doall (cbor/decode codec input)))
(:a 123 true "foo")
```

In this mode, `encode` returns the number of bytes written instead of a byte
array. The sequence returned by `decode` is lazy, so if the input is a file you
must realize the values before closing the input.


## Type Extension

In order to support types of values outside the ones which are a native to CBOR,
the format uses _tagged values_, similar to EDN. In CBOR, the tags are integer
numbers instead of symbols, but the purpose is the same: the tags convey _new
semantics_ about the following value.

The most common example of a need for this kind of type extension is
representing an instant in time. In EDN, this is represented by the `#inst` tag
on an ISO-8601 timestamp string. CBOR offers two tags to represent instants -
tag 0 codes a timestamp string, while tag 1 codes a number in epoch seconds. The
former is more human-friendly, but the latter is more efficient.

New types are implemented by using read and write handlers - functions which map
from typed value to representation and back. This library comes with most of
the tag extensions in the RFC, as well as support for Clojure types like sets,
keywords, and symbols.

### Write Handlers

A write handler is a function with the signature `(f value) => repr`. In almost
all cases the representation is a CBOR tagged value. The tag conveys the type
semantic and generally the expected form that the value takes.

In some cases, multiple types will map to the same tag. For example, by default
this library maps both `java.util.Date` and the newer `java.time.Instant` types
to the same representation.

### Read Handlers

A read handler is a function with the signature `(f tag form) => value`. This
converts the tagged representation back into a typed value. The choice of
function to parse the values determines the 'preferred type' to represent values
of that kind.

Continuing the example, the library comes with read handlers for both `Date` and
`Instant` types, allowing the user to choose their preferred time type.


## Notes

A few things to keep in mind while using the library:

- Decoding half-precision (16-bit) floating-point numbers is supported, but the
  values are promoted to single-precision (32-bit) as the JVM does not have
  native support for them. There is currently no support for writing
  half-precision floats except for special values `+Inf`, `-Inf`, and `NaN`.
- CBOR does not have a type for bare characters, so they will be converted to
  single-character strings when written.
- Sets are currently represented using tag 13, which is not a part of the IANA
  registry.


## License

This is free and unencumbered software released into the public domain.
See the UNLICENSE file for more information.
