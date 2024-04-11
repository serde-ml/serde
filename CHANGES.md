# 0.0.2

Complete rewrite of the Serde library with a heavy focus on usability and
maintainability.

* Support writing manual serializers and deserializers with an ergonomic API.

* Introduce a new CPS-style de/serialization mechanism.

* Configurable de/serializers including options to:
  * rename all fields on records – thanks to @tjdevries :sparkles:
  * ignore unknown fields – thanks to @tjdevries :clap:
  * rename specific fields – thanks to @tjdevries :zap:
  * adjacently tagged variants – thanks to @sabine
  * out of order fields

* Better derivation support now including:
  * records
  * variants (unit, newtype, tuple, and record variants)
  * lists and options
  * floats – thanks to @wesleimp :clap:

* Support nix flakes for development and installation – thanks to @metame :sparkles:

# 0.0.1

Introduce the library with a few standard serde formats:
* debug
* json
* sexpr
* xml
