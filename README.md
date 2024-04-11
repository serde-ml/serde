# serde.ml

A serialization framework for OCaml inspired by
[serde-rs](https://github.com/serde-rs).

The main goals for `serde.ml` are:

* **Serialization** -- take arbitary data structures from the user and turn
  them into specific formats with maximum efficiency.

* **Deserialization** -- read arbitrary data that you parse into data
  structures of the user's choice with maximum efficiency.

```ocaml
type rank = Captain | Chief_petty_officer [@@deriving serialize, deserialize]
type t = { name : string; rank : rank } [@@deriving serialize, deserialize]

let obrien = { name = "Miles O'Brien"; rank = Chief_petty_officer }
let sisko = { name = "Benjamin Sisko"; rank = Captain }

> Serde_json.to_string_pretty (serialize_t) obrien
Ok "{ \"name\": \"Miles O'Brien\", \"rank\": \"Chief_petty_officer\" }"

> Serde_json.of_string (deserialize_t) "{ \"name\": \"Miles O'Brien\", \"rank\": \"Chief_petty_officer\" }"
Ok {name = "Miles O'Brien"; rank = Chief_petty_officer}

> Serde_sexpr.to_string_pretty (serialize_t) obrien;;
Ok "(\"Miles O'Brien\" :Chief_petty_officer)"

> Serde_sexpr.of_string (deserialize_t) "(\"Miles O'Brien\" :Chief_petty_officer)";;
Ok {name = "Miles O'Brien"; rank = Chief_petty_officer}
```

### Installation

To install `serde` from opam, use:

```
; opam install serde serde_derive serde_json -y
```

To install `serde` from sources, use:

```
; opam pin serde.0.0.2 git+https://github.com/serde-ml/serde -y
; opam pin serde_derive.0.0.2 git+https://github.com/serde-ml/serde -y
; opam pin serde_json.0.0.2 git+https://github.com/serde-ml/serde -y
```

### Usage

To derive deserialize/serialize functions for your data types, make sure to

* include the `serde` library
* include `(preprocess (pps serde_derive))` in your library config
* include any of the formats you want to use (like `serde_json`)

Now you can add `@@deriving` annotations to your types:

```ocaml
type rank = Captain | Chief_petty_officer [@@deriving serialize, deserialize]
type t = { name : string; rank : rank } [@@deriving serialize, deserialize]
```

And use them with the formats:

```ocaml
Serde_json.to_string serialize_rank { name = "Benjamin Sisko"; rank = Captain }
```

## Contributing

Check the [CONTRIBUTING.md](./CONTRIBUTING.md) for a small guide on how to
implement new data formats.
