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
        (lazy "<<parse \"(:record (\\\"Benjamin Sisko\\\" 9 \\\"Bajor[...]>>")
      ~tags:[] ~filename:"test/test_de.ml" ~line_number:68 ~start_pos:2
      ~end_pos:150
      (fun () ->
        parse "(:record (\"Benjamin Sisko\" 9 \"Bajor\"))"
          {
            r_name = "Benjamin Sisko";
            r_favorite_number = 9;
            r_location = "Bajor";
          })
end

let () = Ppx_inline_test_lib.Runtime.unset_lib "test_de"
