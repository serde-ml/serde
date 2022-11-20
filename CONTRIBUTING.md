# Contributing

This is a run-of-the-mill Dune project, but we have a tiny Makefile to make
life a little easier.

1. Set up the repo running `make setup` – this will install necessary
   dependencies
2. Run `make` to start the build watcher
3. Run `make test` to run the tests
4. Run `make fmt` to format the code

## Working on `serde_derive`

PPX derivers are notoriously funky to debug, so I've been using the following
command to expand and pretty-print the generated sources:

```sh
; dune describe pp ./examples/starfleet.ml > examples/starfleet_expanded.ml; make fmt;
```

If you're adding some new feature that needs derivation, the workflow I have taken is to:

1. First create a test file in `test` and do the derivation _manually_ – this
   will help you see if the abstraction/API you're building will be complicated
   to do codegen for, and it will give you a working example to test agains.

2. Duplicate your test file, and start implementing the derivation, using the
   `dune describe pp` command above to see that you are getting closer to the
   output you manually expanded.

## Adding data formats

Serde.ml data formats are split into 2 parts: Ser and De.

### Implementing a Data Serializer modules

The Ser module, or Serialization, follows the `Serde.Serializer` interface, and you can create it using the `Serde.Ser.Make` functor like this:

```ocaml
module S = Sexplib.Sexp

let ( let* ) = Result.bind

include Serde.Ser.Make (struct
  type output = S.t
  type error = unit

  let initial_output () = Ok (S.List [])

  let serialize_int _ser _output v = Ok (S.Atom (v |> Int.to_string))
  and serialize_bool _ser _output bool = Ok (S.Atom (Bool.to_string bool))
  and serialize_unit _ser _output () = Ok (S.Atom "()")
  and serialize_char _ser _output char = Ok (S.Atom (String.make 1 char))
  and serialize_float _ser _output float = Ok (S.Atom (Float.to_string float))

  and serialize_string _ser _output string =
    let string =
      if String.contains string ' ' then string else "\"" ^ string ^ "\""
    in
    Ok (S.Atom string)

  and serialize_tuple
      (module Ser : Serde.Ser.Mapper
        with type output = output
         and type error = error) _output ~size:_ ~elements =
    let* parts = Ser.map elements in
    Ok (S.List parts)

  and serialize_unit_variant _ser _output ~type_name:_ ~variant_name
      ~variant_index:_ =
    Ok (S.Atom (":" ^ variant_name))

  and serialize_tuple_variant
      (module Ser : Serde.Ser.Mapper
        with type output = output
         and type error = error) _output ~type_name:_ ~variant_index:_
      ~variant_name ~variant_size:_ ~fields =
    let* fields = Ser.map fields in

    Ok (S.List [ S.Atom (":" ^ variant_name); S.List fields ])

  and serialize_record_variant
      (module Ser : Serde.Ser.Mapper
        with type output = output
         and type error = error) _output ~type_name:_ ~variant_index:_
      ~variant_name ~variant_size:_ ~fields =
    let* fields = Ser.map_field fields in

    let fields = fields |> List.map (fun (_name, sexpr) -> sexpr) in

    Ok (S.List [ S.Atom (":" ^ variant_name); S.List fields ])

  and serialize_record
      (module Ser : Serde.Ser.Mapper
        with type output = output
         and type error = error) _output ~type_name:_ ~record_size:_ ~fields =
    let* fields = Ser.map_field fields in
    let fields = fields |> List.map (fun (_name, sexpr) -> sexpr) in
    Ok (S.List fields)
end)
```

Through all of these methods we thread an `output` that can help you build up
any resulting value. Of course you can ignore it (like we do here) and produce
a fully-immutable value. 

The `Serde.Ser.Mapper` module includes helpful utilities for traversing the
Serde data model while raising errors automatically.

### Implementing a Data Deserializer module

Data deserializers are more complex than their Serialization counterpart, because we want to only read the data that the user requires.

A lot of libraries out there will read a string of JSON in its entirety, build
up all the objects in some JSON representation, and then let you check if those
values fit your types, discarding the unused ones along the way.

Serde.ml tries to do what Serde.rs (from Rust) does: only read what is absolutely necessary.

To implement a Deserializer, you must use the `Serde.De.Make` functor, which
helps you create a factory of deserializers. This factory pattern is used so
that a deserializer can be created with its own internal state.
High-performance parsers and lexers tend to use imperative/mutable APIs, so it
makes sense to accommodate for this.

```ocaml
module Deserializer_factory : Serde.De.Factory = Serde.De.Make (struct
  open Serde.De
  include Serde.De.Unimplemented

  (* define your overrides here *)
end)
```

The `include` we have here is to define some boilerplate deserialization
functions that just error an Unimplemented error message.

I'd avise you to start there, and implement what you need.
