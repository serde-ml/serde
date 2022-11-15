[@@@ocaml.ppx.context
{
  tool_name = "ppx_driver";
  include_dirs = [];
  load_path = [];
  open_modules = [];
  for_package = None;
  debug = false;
  use_threads = false;
  use_vmthreads = false;
  recursive_types = false;
  principal = false;
  transparent_modules = false;
  unboxed_types = false;
  unsafe_string = false;
  cookies = [];
}]

open Serde

type local = bool [@@deriving serializer]

include struct
  let _ = fun (_ : local) -> ()

  let serialize_local t =
    let ( let* ) = Result.bind in
    let* () = Ok () in
    Ser.serialize_bool t
    [@@ocaml.doc " Serialize a value of this type into Serde.data "]

  let _ = serialize_local
end [@@ocaml.doc "@inline"] [@@merlin.hide]

module Other = struct
  type other = int [@@deriving serializer]

  include struct
    let _ = fun (_ : other) -> ()

    let serialize_other t =
      let ( let* ) = Result.bind in
      let* () = Ok () in
      Ser.serialize_int t
      [@@ocaml.doc " Serialize a value of this type into Serde.data "]

    let _ = serialize_other
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
end

type variant =
  | Hello
  | Nested_tuples of (string * int) * bool * unit * Other.other * local
  | World of string * float
  | Salute of { name : string; role : string }
[@@deriving serializer]

include struct
  let _ = fun (_ : variant) -> ()

  let serialize_variant t =
    let ( let* ) = Result.bind in
    let* () = Ok () in
    match t with
    | Hello ->
        Ser.serialize_unit_variant ~typename:"variant" ~variant_idx:1
          ~variant_name:"Hello"
    | Nested_tuples (f_0, f_1, f_2, f_3, f_4) ->
        let* f_0 =
          let f_0, f_1 = f_0 in
          Ser.serialize_tuple ~size:2
            ~elements:[ Ser.serialize_string f_0; Ser.serialize_int f_1 ]
        in
        let* f_1 = Ser.serialize_bool f_1 in
        let* f_2 = Ser.serialize_unit () in
        let* f_3 = Other.serialize_other f_3 in
        let* f_4 = serialize_local f_4 in
        let fields = [ f_0; f_1; f_2; f_3; f_4 ] in
        Ser.serialize_tuple_variant ~typename:"variant" ~variant_idx:2
          ~variant_name:"Nested_tuples" ~variant_size:5 ~fields
    | World (f_0, f_1) ->
        let* f_0 = Ser.serialize_string f_0 in
        let* f_1 = Ser.serialize_float f_1 in
        let fields = [ f_0; f_1 ] in
        Ser.serialize_tuple_variant ~typename:"variant" ~variant_idx:3
          ~variant_name:"World" ~variant_size:2 ~fields
    | Salute { name = f_0; role = f_1 } ->
        let* f_0 = Ser.serialize_string f_0 in
        let* f_1 = Ser.serialize_string f_1 in
        let fields = [ ("name", f_0); ("role", f_1) ] in
        Ser.serialize_record_variant ~typename:"variant" ~variant_idx:4
          ~variant_name:"Salute" ~variant_size:2 ~fields
    [@@ocaml.doc " Serialize a value of this type into Serde.data "]

  let _ = serialize_variant
end [@@ocaml.doc "@inline"] [@@merlin.hide]

type r = { my_field : variant; hello : string } [@@deriving serializer]

include struct
  let _ = fun (_ : r) -> ()

  let serialize_r t =
    let ( let* ) = Result.bind in
    let* () = Ok () in
    let* f_0 = serialize_variant f_0 in
    let* f_1 = Ser.serialize_string f_1 in
    let fields = [ ("my_field", f_0); ("hello", f_1) ] in
    Ser.serialize_record ~typename:"r" ~size:2 ~fields
    [@@ocaml.doc " Serialize a value of this type into Serde.data "]

  let _ = serialize_r
end [@@ocaml.doc "@inline"] [@@merlin.hide]

let test =
  [
    serialize_variant Hello;
    serialize_variant (World ("amazing", 0.0));
    serialize_variant (Salute { name = "sisko"; role = "captain" });
    serialize_r
      {
        my_field = Salute { name = "sisko"; role = "captain" };
        hello = "world";
      };
  ]
