Change Log
==========

All notable changes to this project will be documented in this file, which
follows the conventions of [keepachangelog.com](http://keepachangelog.com/).
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]

...

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

[Unreleased]: https://github.com/greglook/clj-cbor/compare/0.2.0...HEAD
[0.2.0]: https://github.com/greglook/clj-cbor/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/greglook/clj-cbor/tag/0.1.0
