clj-cbor
========

[![CircleCI](https://circleci.com/gh/greglook/clj-cbor.svg?style=shield&circle-token=21efcbc50fe431aa2efc22413ba1f4407fec6283)](https://circleci.com/gh/greglook/clj-cbor)
[![codecov](https://codecov.io/gh/greglook/clj-cbor/branch/develop/graph/badge.svg)](https://codecov.io/gh/greglook/clj-cbor)
[![API documentation](https://img.shields.io/badge/doc-API-blue.svg)](https://greglook.github.io/clj-cbor/api/)
[![Literate documentation](https://img.shields.io/badge/doc-marginalia-blue.svg)](https://greglook.github.io/clj-cbor/marginalia/uberdoc.html)

This library is a native Clojure implementation of the [Concise Binary Object Representation](http://cbor.io/)
format specified in [RFC 7049](https://tools.ietf.org/html/rfc7049).

CBOR is a binary encoding with the goal of small code size, compact messages,
and extensibility without the need for version negotiation. This makes it a good
alternative to [EDN](https://github.com/edn-format/edn) for storing and
transmitting Clojure data in a more compact form.


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
[0 :foo/bar true {:x y} #{1/3} #"foo"]
```

With no extra arguments, `encode` and `decode` will make use of the
`default-codec`, which comes loaded with read and write handler support for many
Java and Clojure types (see the [type extensions](#type-extensions) section
below). Both functions accept an additional argument to specify the codec,
should different behavior be desired.

```clojure
=> (def codec (cbor/cbor-codec :canonical true))

=> (cbor/encode codec {:foo "bar", :baz 123})
; 0xA2D827643A666F6F63626172D827643A62617A187B

=> (cbor/decode codec *1)
{:foo "bar", :baz 123}
```

So far we haven't specified any outputs when encoding, so we've gotten a byte
array back. The full form of `encode` takes three arguments: the codec, the
output stream, and the value to encode.

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
     (doall (cbor/decode-seq codec input)))
(:a 123 true "foo")
```

In this mode, `encode` returns the number of bytes written instead of a byte
array. We can read multiple items from an input stream using `decode-seq`, which
returns a lazy sequence. If the input is a file you must realize the values
before closing the input. Similarly, `encode-seq` will write a sequence of
multiple values to an output stream.

As a convenience, the library also provides the `spit`, `spit-all`, `slurp`, and
`slurp-all` functions, which operate on files:

```clojure
=> (cbor/spit "data.cbor" {:abc 123, :foo "qux", :bar true})
29

=> (cbor/spit-all "data.cbor" [[0.0 'x] #{-42}] :append true)
12

=> (.length (io/file "data.cbor"))
41

=> (cbor/slurp "data.cbor")
{:abc 123, :bar true, :foo "qux"}

=> (cbor/slurp-all "data.cbor")
({:abc 123, :bar true, :foo "qux"} [0.0 x] #{-42})
```


## Type Extensions

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
from typed value to representation and back. Currently, the library comes with
support for the following types:

|   Tag | Representation | Type | Semantics |
|-------|----------------|------|-----------|
|     0 | Text string    | `Date`/`Instant` | Standard date/time string |
|     1 | Number         | `Date`/`Instant` | Epoch-based date/time |
|     2 | Byte string    | `BigInt` | Positive bignum |
|     3 | Byte string    | `BigInt` | Negative bignum |
|     4 | Array(2)       | `BigDecimal` | Decimal fraction |
|    27 | Array(2)       | `TaggedLiteral` | Constructor support for Clojure tagged literal values |
|    30 | Array(2)       | `Ratio` | Rational fractions, represented as numberator and denominator numbers |
|    32 | Text string    | `URI` | Uniform Resource Identifier strings |
|    35 | Text string    | `Pattern` | Regular expression strings |
|    37 | Byte string    | `UUID` | Binary-encoded UUID values |
|    39 | Text string    | `Symbol`/`Keyword` | Identifiers |
|   258 | Array          | `Set` | Sets of unique entries |
| 55799 | Varies         | N/A | Self-describe CBOR |

For further information about registered tag semantics, consult the
[IANA Registry](https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml).

### Write Handlers

A write handler is a function which takes a typed value and returns an encodable
representation. In most cases the representation is a CBOR tagged value. The tag
conveys the type semantic and generally the expected form that the
representation takes.

In some cases, multiple types will map to the same tag. For example, by default
this library maps both `java.util.Date` and the newer `java.time.Instant` types
to the same representation.

### Read Handlers

A read handler is a function which takes the representation from a tagged value
and returns an appropriately typed value. The choice of function to parse the
values thus determines the 'preferred type' to represent values of that kind.

Continuing the example, the library comes with read handlers for both `Date` and
`Instant` types, allowing the user to choose their preferred time type.


## Performance

As of `0.7.0`, this library is competitive with many other comparable
serialization formats. Some benchmarking results can be found in
[this spreadsheet](https://docs.google.com/spreadsheets/d/142LhWX5aCnOoF6v0T46RASULQDuG7JIckKiCohDPgq8/edit?usp=sharing).

For small and medium data sizes CBOR is more compact than most formats, while at
larger sizes (above 512 bytes) all formats are fairly close in size (within 10%,
generally). Other than Nippy, which was by far the fastest codec, clj-cbor
was one of the fastest encoders and is in the middle of the pack in decoding
times.

To give some concrete performance numbers, here are a few samples from the
dataset:

| Size |    Encode |    Decode |
|------|-----------|-----------|
|    4 |   6.09 µs |   2.31 µs |
|   55 |  20.87 µs |   7.58 µs |
|  173 |  12.64 µs |   5.74 µs |
|  388 |  15.38 µs |  11.60 µs |
|  882 |  31.55 µs |  14.24 µs |
| 1632 |  54.82 µs |  33.52 µs |
| 3127 |  92.14 µs |  64.66 µs |
| 4918 | 104.92 µs |  59.67 µs |
| 7328 | 108.37 µs |  82.16 µs |


## Notes

A few things to keep in mind while using the library:

- Streaming CBOR data can be parsed from input, but there is currently no way to
  generate streaming output data.
- Decoding half-precision (16-bit) floating-point numbers is supported, but the
  values are promoted to single-precision (32-bit) as the JVM does not have
  native support for them. There is currently no support for writing
  half-precision floats except for the special values `0.0`, `+Inf`, `-Inf`, and
  `NaN`, which are always written as two bytes for efficiency.
- CBOR does not have a type for bare characters, so they will be converted to
  single-character strings when written.
- Regular expressions are supported using tag 35, but beware that Java
  `Pattern` objects do not compare equal or have the same hash code for
  otherwise identical regexes. Using them in sets or as map keys is discouraged.


## License

This is free and unencumbered software released into the public domain.
See the UNLICENSE file for more information.
