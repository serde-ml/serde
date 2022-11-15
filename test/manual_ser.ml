(*
open Serde

module Test_int = struct
  type number = int

  let serialize t = Ser.serialize_int t
end

module Test_variant = struct
  type variant =
    | Hello
    | World of string
    | Goodbye of { first_name : string; last_name : string }

  let __serde__typename = "variant"

  let serialize t =
    let ( let* ) = Result.bind in
    match t with
    | Hello ->
        Ser.serialize_unit_variant ~typename:__serde__typename ~variant_idx:0
          ~variant_name:"Hello"
    | World f0 ->
        let* f0 = Ser.serialize_string f0 in
        let fields = [ f0 ] in
        Ser.serialize_tuple_variant ~typename:__serde__typename ~variant_idx:1
          ~variant_name:"World" ~variant_size:1 ~fields
    | Goodbye { first_name: f0; last_name: f1 } ->
        let* f0 = Ser.serialize_string f0 in
        let* f1 = Ser.serialize_string f1 in
        let fields = [ ("first_name", f0); ("last_name", f1) ] in
        Ser.serialize_record_variant ~typename:__serde__typename ~variant_idx:2
          ~variant_name:"Goodbye" ~variant_size:2 ~fields
end

module Test_record = struct
  type record = { name : string; kind : Test_variant.variant }

  let __serde__typename = "record"

  let serialize t =
    let ( let* ) = Result.bind in
    let* f0 = Ser.serialize_string t.name in
    let* f1 = Test_variant.serialize t.kind in
    let fields = [ ("name", f0); ("kind", f1) ] in
    Ser.serialize_record ~typename:__serde__typename ~size:2 ~fields
end
*)
