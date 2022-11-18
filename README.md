# serde.ml

A serialization framework for OCaml inspired by [serde-rs](https://github.com/serde-rs).

The main goals for `serde.ml` are:

* **Serialization** -- take arbitary data structures from the user and turn them into specific formats with maximum efficiency.
* **Deserialization** -- read arbitrary data that you parse into data structures of the user's choice with maximum efficiency.

> NOTE: this is super not ready for production yet

### Usage

Set up the `serde_derive` ppx, and bring in any data format modules you want to use. Here we bring s-expressions and json.

```dune
(library
 (name my_lib)
 (preprocess (pps serde_derive))
 (libraries serde serde_derive serde_sexpr serde_json))
```

Tag your data structures with `deriving (serializer, deserializer)`.

```ocaml
open Serde

type t =
  | Hello
  | Tuple1 of string
  | Tuple2 of string * bool
  | Record3 of { name : string; favorite_number : int; location : string }
[@@deriving (serializer, deserializer)]
```

Now you have a `serialize_{typeName}` and `deserialize_{typeName}` functions that you can pass into the different data format modules.

To read data, use `deserialize_t` like this:

```ocaml
let sexpr = "(:Record3 (\"Benjamin Sisko\" 9 \"Bajor\"))" in
let* t = Serde_sexpr.of_string deserialize_t sexpr in
t == (Record3 { name = "Benjamin Sisko"; favorite_number = 9; location = "Bajor" })
```

To render data, use `serialize_t` like this:

```ocaml
let t = (Record3 { name = "Benjamin Sisko"; favorite_number = 9; location = "Bajor" }) in
let* sexpr = Serde_sexpr.to_string_pretty serialize_t t in
sexpr == "(:Record3 (\"Benjamin Sisko\" 9 \"Bajor\"))"
```

To transcode data across formats, switch the data module:

```ocaml
(* read sexpr *)
let sexpr = "(:Record3 (\"Benjamin Sisko\" 9 \"Bajor\"))" in
let* t = Serde_sexpr.of_string deserialize_t sexpr in
(* write json *)
let* json = Serde_json.to_string_pretty serialize_t t in
json == "{
  \"t#Record3\": {
    \"name\": \"Benjamin Sisko\",
    \"favorite_number\": 9,
    \"location\": \"Deep Space 9\"
  }
}"
```
