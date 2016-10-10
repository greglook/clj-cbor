clj-cbor
========

A native Clojure implementation of the [Concise Binary Object Representation](http://cbor.io/)
specification.

## Usage

**This library is still a work in progress.**

## Notes

A few things to keep in mind while using the library:

- Decoding half-precision (16-bit) floating-point numbers is supported, but the
  values are promoted to single-precision (32-bit) as the JVM does not have
  native support for them. There is currently no support for writing
  half-precision floats.

## License

This is free and unencumbered software released into the public domain.
See the UNLICENSE file for more information.
