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
  cookies = [ ("inline_tests", "enabled"); ("library-name", "test_de") ];
}]

let () = Ppx_inline_test_lib.Runtime.set_lib_and_partition "test_de" ""

open Serde

let ( let* ) = Result.bind

let parse eq fn s t =
  match Serde_sexpr.of_string fn s |> Result.map_error (fun x -> `De x) with
  | Ok t' -> eq t t'
  | Error (`De (`Unimplemented msg)) ->
      print_string ("unimplemented: " ^ msg);
      false
  | Error (`De (`Invalid_field_index _)) ->
      print_string "invalid_field_idx";
      false
  | Error (`De (`Unknown_field s)) ->
      print_string ("Unknown_field: " ^ s);
      false
  | Error (`De (`Invalid_variant_index _)) ->
      print_string "invalid_va_idx";
      false
  | Error (`De (`Unknown_variant s)) ->
      print_string ("Unknown_variant: " ^ s);
      false
  | Error (`De (`Duplicate_field _)) ->
      print_string "Duplicate_field";
      false
  | Error (`De (`Missing_field _)) ->
      print_string "Missing_field";
      false
  | Error (`De (`Message msg)) ->
      print_string ("msg: " ^ msg);
      false
  | Error (`Ser _) ->
      print_string "error serializing";
      false

module Type_alias = struct
  type alias = int [@@deriving eq, serializer, deserializer]

  let rec equal_alias : alias -> alias -> Ppx_deriving_runtime.bool =
    ((let open! Ppx_deriving_runtime [@ocaml.warning "-A"] in
     fun (a : int) b -> a = b) [@ocaml.warning "-A"])
    [@@ocaml.warning "-39"]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : alias) -> ()

    let serialize_alias t =
      let ( let* ) = Result.bind in
      let* () = Ok () in
      Ser.serialize_int t
      [@@ocaml.doc " Serialize a value of this type into Serde.data "]

    let _ = serialize_alias

    module Serde_deserialize_alias = struct
      let deserialize_alias (module De : Serde.De.Deserializer) =
        Serde.De.deserialize_int (module De) (module Serde.De.Impls.Int_visitor)

      let _ = deserialize_alias
    end

    include Serde_deserialize_alias
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let parse = parse Int.equal deserialize_alias

  let () =
    Ppx_inline_test_lib.Runtime.test
      ~config:(module Inline_test_config)
      ~descr:(lazy "<<parse \"1\" 1>>")
      ~tags:[] ~filename:"test/test_de.ml" ~line_number:41 ~start_pos:2
      ~end_pos:26
      (fun () -> parse "1" 1)
end

module Type_tuple = struct
  type tuple = int * Type_alias.alias * bool [@@deriving eq, deserializer]

  let rec equal_tuple : tuple -> tuple -> Ppx_deriving_runtime.bool =
    let __0 () = Type_alias.equal_alias in
    ((let open! Ppx_deriving_runtime [@ocaml.warning "-A"] in
     fun (lhs0, lhs1, lhs2) (rhs0, rhs1, rhs2) ->
       ((fun (a : int) b -> a = b) lhs0 rhs0 && (fun x -> (__0 ()) x) lhs1 rhs1)
       && (fun (a : bool) b -> a = b) lhs2 rhs2) [@ocaml.warning "-A"])
    [@@ocaml.warning "-39"]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : tuple) -> ()

    module Serde_deserialize_tuple = struct
      module Abstract_tuple_visitor_for_tuple :
        Serde.De.Visitor.Intf with type value = tuple =
      Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = tuple
        type tag = unit

        let visit_seq :
            (module Serde.De.Visitor.Intf with type value = value) ->
            (module Serde.De.Deserializer) ->
            (value, 'error) Serde.De.Sequence_access.t ->
            (value, 'error Serde.De.Error.de_error) result =
         fun (module Self) (module De) seq_access ->
          let* f_0 =
            let deser_element () =
              Serde.De.deserialize_int
                (module De)
                (module Serde.De.Impls.Int_visitor)
            in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message (Printf.sprintf "tuple needs 3 argument")
            | Some f0 -> Ok f0
          in
          let* f_1 =
            let deser_element () = Type_alias.deserialize_alias (module De) in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message (Printf.sprintf "tuple needs 3 argument")
            | Some f0 -> Ok f0
          in
          let* f_2 =
            let deser_element () =
              Serde.De.deserialize_bool
                (module De)
                (module Serde.De.Impls.Bool_visitor)
            in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message (Printf.sprintf "tuple needs 3 argument")
            | Some f0 -> Ok f0
          in
          Ok (f_0, f_1, f_2)

        let _ = visit_seq
      end)

      let deserialize_tuple (module De : Serde.De.Deserializer) =
        Serde.De.deserialize_seq
          (module De)
          (module Abstract_tuple_visitor_for_tuple)

      let _ = deserialize_tuple
    end

    include Serde_deserialize_tuple
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let parse = parse equal_tuple deserialize_tuple

  let () =
    Ppx_inline_test_lib.Runtime.test
      ~config:(module Inline_test_config)
      ~descr:(lazy "<<parse \"(21 12 true)\" (21, 12, true)>>")
      ~tags:[] ~filename:"test/test_de.ml" ~line_number:55 ~start_pos:2
      ~end_pos:50
      (fun () -> parse "(21 12 true)" (21, 12, true))
end

module Type_record = struct
  type record = {
    r_name : string;
    r_favorite_number : int;
    r_location : string;
  }
  [@@deriving eq, serializer, deserializer]

  let rec equal_record : record -> record -> Ppx_deriving_runtime.bool =
    ((let open! Ppx_deriving_runtime [@ocaml.warning "-A"] in
     fun lhs rhs ->
       ((fun (a : string) b -> a = b) lhs.r_name rhs.r_name
       && (fun (a : int) b -> a = b) lhs.r_favorite_number rhs.r_favorite_number
       )
       && (fun (a : string) b -> a = b) lhs.r_location rhs.r_location)
    [@ocaml.warning "-A"])
    [@@ocaml.warning "-39"]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : record) -> ()

    let serialize_record t =
      let ( let* ) = Result.bind in
      let* () = Ok () in
      let { r_name = f_0; r_favorite_number = f_1; r_location = f_2 } = t in
      let* f_0 = Ser.serialize_string f_0 in
      let* f_1 = Ser.serialize_int f_1 in
      let* f_2 = Ser.serialize_string f_2 in
      let fields =
        [ ("r_name", f_0); ("r_favorite_number", f_1); ("r_location", f_2) ]
      in
      Ser.serialize_record ~typename:"record" ~size:3 ~fields
      [@@ocaml.doc " Serialize a value of this type into Serde.data "]

    let _ = serialize_record

    module Serde_deserialize_record = struct
      let name = "record"
      let _ = name
      let fields = [ "r_name"; "r_favorite_number"; "r_location" ]
      let _ = fields

      type nonrec fields =
        | Field_r_name
        | Field_r_favorite_number
        | Field_r_location

      module Field_visitor_for_record = Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = fields
        type tag = unit

        let visit_int idx =
          match idx with
          | 0 -> Ok Field_r_name
          | 1 -> Ok Field_r_favorite_number
          | 2 -> Ok Field_r_location
          | _ -> Serde.De.Error.invalid_variant_index ~idx

        let _ = visit_int

        let visit_string str =
          match str with
          | "r_name" -> Ok Field_r_name
          | "r_favorite_number" -> Ok Field_r_favorite_number
          | "r_location" -> Ok Field_r_location
          | _ -> Serde.De.Error.unknown_variant str

        let _ = visit_string
      end)

      module Visitor_for_record = Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = record
        type tag = fields

        let visit_seq :
            (module Serde.De.Visitor.Intf with type value = value) ->
            (module Serde.De.Deserializer) ->
            (value, 'error) Serde.De.Sequence_access.t ->
            (value, 'error Serde.De.Error.de_error) result =
         fun (module Self) (module De) seq_access ->
          let* f_0 =
            let deser_element () =
              Serde.De.deserialize_string
                (module De)
                (module Serde.De.Impls.String_visitor)
            in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message
                  (Printf.sprintf "record needs 3 argument")
            | Some f0 -> Ok f0
          in
          let* f_1 =
            let deser_element () =
              Serde.De.deserialize_int
                (module De)
                (module Serde.De.Impls.Int_visitor)
            in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message
                  (Printf.sprintf "record needs 3 argument")
            | Some f0 -> Ok f0
          in
          let* f_2 =
            let deser_element () =
              Serde.De.deserialize_string
                (module De)
                (module Serde.De.Impls.String_visitor)
            in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message
                  (Printf.sprintf "record needs 3 argument")
            | Some f0 -> Ok f0
          in
          Ok { r_name = f_0; r_favorite_number = f_1; r_location = f_2 }

        let _ = visit_seq
      end)

      let deserialize_record (module De : Serde.De.Deserializer) =
        Serde.De.deserialize_record ~name ~fields
          (module De)
          (module Visitor_for_record)
          (module Field_visitor_for_record)

      let _ = deserialize_record
    end

    include Serde_deserialize_record
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let parse = parse equal_record deserialize_record

  let () =
    Ppx_inline_test_lib.Runtime.test
      ~config:(module Inline_test_config)
      ~descr:
        (lazy "<<parse \"(:record \\\"Benjamin Sisko\\\" 9 \\\"Bajor\\[...]>>")
      ~tags:[] ~filename:"test/test_de.ml" ~line_number:68 ~start_pos:2
      ~end_pos:148
      (fun () ->
        parse "(:record \"Benjamin Sisko\" 9 \"Bajor\")"
          {
            r_name = "Benjamin Sisko";
            r_favorite_number = 9;
            r_location = "Bajor";
          })
end

module Type_variant = struct
  type variant =
    | Hello
    | Tuple1 of string
    | Tuple2 of string * Type_alias.alias
    | Record3 of { name : string; favorite_number : int; location : string }
  [@@deriving eq, serializer, deserializer]

  let rec equal_variant : variant -> variant -> Ppx_deriving_runtime.bool =
    let __0 () = Type_alias.equal_alias in
    ((let open! Ppx_deriving_runtime [@ocaml.warning "-A"] in
     fun lhs rhs ->
       match (lhs, rhs) with
       | Hello, Hello -> true
       | Tuple1 lhs0, Tuple1 rhs0 -> (fun (a : string) b -> a = b) lhs0 rhs0
       | Tuple2 (lhs0, lhs1), Tuple2 (rhs0, rhs1) ->
           (fun (a : string) b -> a = b) lhs0 rhs0
           && (fun x -> (__0 ()) x) lhs1 rhs1
       | ( Record3
             {
               name = lhsname;
               favorite_number = lhsfavorite_number;
               location = lhslocation;
             },
           Record3
             {
               name = rhsname;
               favorite_number = rhsfavorite_number;
               location = rhslocation;
             } ) ->
           ((fun (a : string) b -> a = b) lhsname rhsname
           && (fun (a : int) b -> a = b) lhsfavorite_number rhsfavorite_number)
           && (fun (a : string) b -> a = b) lhslocation rhslocation
       | _ -> false) [@ocaml.warning "-A"])
    [@@ocaml.warning "-39"]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : variant) -> ()

    let serialize_variant t =
      let ( let* ) = Result.bind in
      let* () = Ok () in
      match t with
      | Hello ->
          Ser.serialize_unit_variant ~typename:"variant" ~variant_idx:1
            ~variant_name:"Hello"
      | Tuple1 f_0 ->
          let* f_0 = Ser.serialize_string f_0 in
          let fields = [ f_0 ] in
          Ser.serialize_tuple_variant ~typename:"variant" ~variant_idx:2
            ~variant_name:"Tuple1" ~variant_size:1 ~fields
      | Tuple2 (f_0, f_1) ->
          let* f_0 = Ser.serialize_string f_0 in
          let* f_1 = Type_alias.serialize_alias f_1 in
          let fields = [ f_0; f_1 ] in
          Ser.serialize_tuple_variant ~typename:"variant" ~variant_idx:3
            ~variant_name:"Tuple2" ~variant_size:2 ~fields
      | Record3 { name = f_0; favorite_number = f_1; location = f_2 } ->
          let* f_0 = Ser.serialize_string f_0 in
          let* f_1 = Ser.serialize_int f_1 in
          let* f_2 = Ser.serialize_string f_2 in
          let fields =
            [ ("name", f_0); ("favorite_number", f_1); ("location", f_2) ]
          in
          Ser.serialize_record_variant ~typename:"variant" ~variant_idx:4
            ~variant_name:"Record3" ~variant_size:3 ~fields
      [@@ocaml.doc " Serialize a value of this type into Serde.data "]

    let _ = serialize_variant

    module Serde_deserialize_variant = struct
      let name = "variant"
      let _ = name
      let variants = [ "Hello"; "Tuple1"; "Tuple2"; "Record3" ]
      let _ = variants

      type nonrec variants =
        | Field_Hello
        | Field_Tuple1
        | Field_Tuple2
        | Field_Record3

      module Tag_visitor_for_variant = Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = variants
        type tag = unit

        let visit_int idx =
          match idx with
          | 0 -> Ok Field_Hello
          | 1 -> Ok Field_Tuple1
          | 2 -> Ok Field_Tuple2
          | 3 -> Ok Field_Record3
          | _ -> Serde.De.Error.invalid_variant_index ~idx

        let _ = visit_int

        let visit_string str =
          match str with
          | "Hello" -> Ok Field_Hello
          | "Tuple1" -> Ok Field_Tuple1
          | "Tuple2" -> Ok Field_Tuple2
          | "Record3" -> Ok Field_Record3
          | _ -> Serde.De.Error.unknown_variant str

        let _ = visit_string
      end)

      module Variant_visitor_for_Hello :
        Serde.De.Visitor.Intf with type value = variant =
      Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = variant
        type tag = unit
      end)

      module Variant_visitor_for_Tuple1 :
        Serde.De.Visitor.Intf with type value = variant =
      Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = variant
        type tag = unit

        let visit_seq :
            (module Serde.De.Visitor.Intf with type value = value) ->
            (module Serde.De.Deserializer) ->
            (value, 'error) Serde.De.Sequence_access.t ->
            (value, 'error Serde.De.Error.de_error) result =
         fun (module Self) (module De) seq_access ->
          let* f_0 =
            let deser_element () =
              Serde.De.deserialize_string
                (module De)
                (module Serde.De.Impls.String_visitor)
            in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message
                  (Printf.sprintf "Tuple1 needs 1 argument")
            | Some f0 -> Ok f0
          in
          Ok (Tuple1 f_0)

        let _ = visit_seq
      end)

      module Variant_visitor_for_Tuple2 :
        Serde.De.Visitor.Intf with type value = variant =
      Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = variant
        type tag = unit

        let visit_seq :
            (module Serde.De.Visitor.Intf with type value = value) ->
            (module Serde.De.Deserializer) ->
            (value, 'error) Serde.De.Sequence_access.t ->
            (value, 'error Serde.De.Error.de_error) result =
         fun (module Self) (module De) seq_access ->
          let* f_0 =
            let deser_element () =
              Serde.De.deserialize_string
                (module De)
                (module Serde.De.Impls.String_visitor)
            in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message
                  (Printf.sprintf "Tuple2 needs 2 argument")
            | Some f0 -> Ok f0
          in
          let* f_1 =
            let deser_element () = Type_alias.deserialize_alias (module De) in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message
                  (Printf.sprintf "Tuple2 needs 2 argument")
            | Some f0 -> Ok f0
          in
          Ok (Tuple2 (f_0, f_1))

        let _ = visit_seq
      end)

      module Variant_visitor_for_Record3 :
        Serde.De.Visitor.Intf with type value = variant =
      Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = variant
        type tag = unit

        let visit_seq :
            (module Serde.De.Visitor.Intf with type value = value) ->
            (module Serde.De.Deserializer) ->
            (value, 'error) Serde.De.Sequence_access.t ->
            (value, 'error Serde.De.Error.de_error) result =
         fun (module Self) (module De) seq_access ->
          let* f_0 =
            let deser_element () =
              Serde.De.deserialize_string
                (module De)
                (module Serde.De.Impls.String_visitor)
            in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message
                  (Printf.sprintf "Record3 needs 3 argument")
            | Some f0 -> Ok f0
          in
          let* f_1 =
            let deser_element () =
              Serde.De.deserialize_int
                (module De)
                (module Serde.De.Impls.Int_visitor)
            in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message
                  (Printf.sprintf "Record3 needs 3 argument")
            | Some f0 -> Ok f0
          in
          let* f_2 =
            let deser_element () =
              Serde.De.deserialize_string
                (module De)
                (module Serde.De.Impls.String_visitor)
            in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message
                  (Printf.sprintf "Record3 needs 3 argument")
            | Some f0 -> Ok f0
          in
          Ok (Record3 { name = f_0; favorite_number = f_1; location = f_2 })

        let _ = visit_seq
      end)

      module Visitor_for_variant :
        Serde.De.Visitor.Intf with type value = variant and type tag = variants =
      Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = variant
        type tag = variants

        let visit_variant va =
          let* tag = Serde.De.Variant_access.tag va in
          match tag with
          | Field_Hello ->
              let* () = Serde.De.Variant_access.unit_variant va in
              Ok Hello
          | Field_Tuple1 ->
              Serde.De.Variant_access.tuple_variant va
                (module Variant_visitor_for_Tuple1)
          | Field_Tuple2 ->
              Serde.De.Variant_access.tuple_variant va
                (module Variant_visitor_for_Tuple2)
          | Field_Record3 ->
              Serde.De.Variant_access.record_variant va
                (module Variant_visitor_for_Record3)

        let _ = visit_variant
      end)

      let deserialize_variant (module De : Serde.De.Deserializer) =
        Serde.De.deserialize_variant ~name ~variants
          (module De)
          (module Visitor_for_variant)
          (module Tag_visitor_for_variant)

      let _ = deserialize_variant
    end

    include Serde_deserialize_variant
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let parse = parse equal_variant deserialize_variant

  let () =
    Ppx_inline_test_lib.Runtime.test
      ~config:(module Inline_test_config)
      ~descr:(lazy "<<parse \":Hello\" Hello>>")
      ~tags:[] ~filename:"test/test_de.ml" ~line_number:83 ~start_pos:2
      ~end_pos:35
      (fun () -> parse ":Hello" Hello)

  let () =
    Ppx_inline_test_lib.Runtime.test
      ~config:(module Inline_test_config)
      ~descr:
        (lazy "<<parse \"(:Tuple1 (\\\"this is a tuple\\\"))\" (Tupl[...]>>")
      ~tags:[] ~filename:"test/test_de.ml" ~line_number:85 ~start_pos:2
      ~end_pos:85
      (fun () ->
        parse "(:Tuple1 (\"this is a tuple\"))" (Tuple1 "this is a tuple"))

  let () =
    Ppx_inline_test_lib.Runtime.test
      ~config:(module Inline_test_config)
      ~descr:
        (lazy "<<parse \"(:Tuple2 (\\\"this is a tuple\\\"  true))\"[...]>>")
      ~tags:[] ~filename:"test/test_de.ml" ~line_number:88 ~start_pos:2
      ~end_pos:105
      (fun () ->
        parse "(:Tuple2 (\"this is a tuple\"  true))"
          (Tuple2 ("this is a tuple", true)))

  let () =
    Ppx_inline_test_lib.Runtime.test
      ~config:(module Inline_test_config)
      ~descr:
        (lazy "<<parse \"(:Record3 (\\\"Benjamin Sisko\\\" 9 \\\"Bajo[...]>>")
      ~tags:[] ~filename:"test/test_de.ml" ~line_number:92 ~start_pos:2
      ~end_pos:164
      (fun () ->
        parse "(:Record3 (\"Benjamin Sisko\" 9 \"Bajor\"))"
          (Record3
             {
               name = "Benjamin Sisko";
               favorite_number = 9;
               location = "Bajor";
             }))
end

let () = Ppx_inline_test_lib.Runtime.unset_lib "test_de"
