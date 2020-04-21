Change Log
==========

All notable changes to this project will be documented in this file, which
follows the conventions of [keepachangelog.com](http://keepachangelog.com/).
This project adheres to [Semantic Versioning](http://semver.org/).


## [Unreleased]

### Added
- Errors about unhandled types now contain more type information about the
  value that could not be encoded.
  [#15](https://github.com/greglook/clj-cbor/pull/15)


## [0.7.2] - 2019-06-28

### Added
- Add [clj-async-profiler](https://github.com/clojure-goes-fast/clj-async-profiler)
  support in the benchmarking code to produce flame graphs for finding hotspots.

### Removed
- Codecs no longer support a configurable set tag; it is now fixed at 258
  matching the IANA registry.

### Fixed
- `decode` will not try to coerce inputs which are already input streams
  anymore. This fixes incremental/lazy decoding behavior.
  [#13](https://github.com/greglook/clj-cbor/issues/13)
- `encode` will not try to coerce outputs to an output stream anymore. Now an
  exception is thrown when a bad argument is given. This prevents lost writes
  when the final buffer is not flushed correctly.
  [#14](https://github.com/greglook/clj-cbor/pull/14)


## [0.7.1] - 2019-01-10

This is a performance-focused release that introduced a number of changes to
speed up decoding performance. The main change is the switch to a _jump table_
for the initial header byte.
[#9](https://github.com/greglook/clj-cbor/issues/9)
[#11](https://github.com/greglook/clj-cbor/pull/11)

For a representative data sample of about ~97 KB, this brings the benchmarked
decoding time from 8.454 ms to 4.089 ms, or about twice as fast!

### Changed
- Upgrade to Clojure 1.10.0.
- Many operations in the codec are type-hinted to use primitive operations where
  possible.
- CBOR arrays and maps are built up using transients for performance.
- Decoding logic now uses a jump table.

### Fixed
- A tagged-value major type with a streaming info code now results in a
  `::codec/illegal-stream` error.


## [0.6.0] - 2017-12-23

### Changed
- Upgrade to Clojure 1.9.0.


## [0.5.0] - 2017-11-08

This release fixes two of the longer-standing quirks with the library, which
were unfortunately breaking changes. The fixes should be straightforward:

- Replace any `(cbor/decode ...)` with `(cbor/decode-seq ...)`.
- Replace any `(first (cbor/decode-seq ...))` with `(cbor/decode ...)`.

If you have existing encoded data containing sets, you can use the following
function to rewrite it:

```clojure
(defn rewrite-cbor-sets
  [codec source dest]
  (with-open [input (io/input-stream source)
              output (io/output-stream dest)]
    (->>
      input
      (cbor/decode-seq (assoc codec :set-tag 13))
      (cbor/encode-seq codec output))))
```

If rewriting isn't an option, you can support reading sets via tag 13 by
using a custom read handler:

```clojure
(def compatible-codec
  (assoc-in cbor/default-codec [:read-handlers 13] set))
```

### Changed
- **Breaking:** the default set tag is now 258, matching the IANA registry.
  [#6](//github.com/greglook/clj-cbor/issues/6)
- **Breaking:** `clj-cbor.core/decode` now only decodes a single value; previous
  behavior moved to `decode-seq`.
  [#7](//github.com/greglook/clj-cbor/issues/7)

### Added
- `clj-cbor.core/encode-seq` writes a sequence of values to a byte array or
  output stream.
- `clj-cbor.core/spit-all` similarly writes a sequence of values to an output
  file like repeated calls to `spit` with `:append true`.


## [0.4.1] - 2017-05-17

### Fixed
- Resolved an overflow issue when encoding `Long/MIN_VALUE`.
- Integer values are always decoded as `Long` values, no matter their encoded
  size. Previously, numbers between `Short/MAX_VALUE` and `Integer/MAX_VALUE`
  would return `Integer` values.
  [#4](https://github.com/greglook/clj-cbor/issues/4)
  [#5](https://github.com/greglook/clj-cbor/pull/5)


## [0.4.0] - 2017-03-14

### Added
- Implemented canonical mode sorting of map keys and set entries.


## [0.3.0] - 2016-01-05

### Added
- Support self-describe CBOR tag 55799. This provides a 'magic' three-byte
  sequence to simplify detection of the CBOR format. The
  `clj-cbor.core/self-describe` function will wrap the given value with this
  tag.
- Tag codes have been factored out into integer constants where appropriate to
  improve consistency.
- Add `spit`, `slurp`, and `slurp-all` utility functions to the core namespace.

### Changed
- Read handler functions are no longer called with the tag. This greatly
  simplifies their implementation and allows for reuse of existing
  transformation functions as-is.
- Error type `:clj-cbor.codec/illegal-chunk` renamed to `illegal-chunk-type`.

### Fixed
- The `decode` function now properly raises an error when the input ends
  mid-value rather than at a top-level value boundary.


## [0.2.0] - 2016-12-28

This release includes 100% test coverage!

### Added
- UUIDs are supported in binary form using tag 37.
- CBOR error keywords are organized into a hierarchy underneath
  `:clj-cbor.error/encoding-error` and `:clj-cbor.error/decoding-error`.

### Changed
- `clj-cbor.data.model` renamed to `clj-cbor.data.core`.
- The `clj-cbor.float16` functions `from-bits` and `to-bits` renamed to
  `decode` and `encode`, respectively.
- `Undefined` and `SimpleValue` records moved to new `clj-cbor.data.simple`
  namespace.
- `TaggedValue` record moved to new `clj-cbor.data.tagged` namespace.
- `clj-cbor.header/write-major-int` renamed to `write`.
- `clj-cbor.header/read-size` renamed to `read-code`.

### Fixed
- Generally tighted up namespaces and reduced linkage where possible.


## [0.1.0] - 2016-12-23

Initial project release.


[Unreleased]: https://github.com/greglook/clj-cbor/compare/0.7.2...HEAD
[0.7.2]: https://github.com/greglook/clj-cbor/compare/0.7.1...0.7.2
[0.7.1]: https://github.com/greglook/clj-cbor/compare/0.6.0...0.7.1
[0.6.0]: https://github.com/greglook/clj-cbor/compare/0.5.0...0.6.0
[0.5.0]: https://github.com/greglook/clj-cbor/compare/0.4.1...0.5.0
[0.4.1]: https://github.com/greglook/clj-cbor/compare/0.4.0...0.4.1
[0.4.0]: https://github.com/greglook/clj-cbor/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/greglook/clj-cbor/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/greglook/clj-cbor/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/greglook/clj-cbor/tag/0.1.0
