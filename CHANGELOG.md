Change Log
==========

All notable changes to this project will be documented in this file, which
follows the conventions of [keepachangelog.com](http://keepachangelog.com/).
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]

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

[Unreleased]: https://github.com/greglook/clj-cbor/compare/0.3.0...HEAD
[0.3.0]: https://github.com/greglook/clj-cbor/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/greglook/clj-cbor/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/greglook/clj-cbor/tag/0.1.0
