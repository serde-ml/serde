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
  cookies = [ ("library-name", "examples") ];
}]

open Serde

module Starfleet_staff = struct
  type rank = Captain | Chief_petty_officer
  [@@deriving serializer, deserializer]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : rank) -> ()

    let serialize_rank t =
      let ( let* ) = Result.bind in
      let* () = Ok () in
      match t with
      | Captain ->
          Ser.serialize_unit_variant ~typename:"rank" ~variant_idx:1
            ~variant_name:"Captain"
      | Chief_petty_officer ->
          Ser.serialize_unit_variant ~typename:"rank" ~variant_idx:2
            ~variant_name:"Chief_petty_officer"
      [@@ocaml.doc " Serialize a value of this type into Serde.data "]

    let _ = serialize_rank

    module Serde_deserialize_rank = struct
      let name = "rank"
      let _ = name
      let variants = [ "Captain"; "Chief_petty_officer" ]
      let _ = variants

      type nonrec variants = Field_Captain | Field_Chief_petty_officer

      module Tag_visitor_for_rank = Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = variants
        type tag = unit

        let visit_int idx =
          match idx with
          | 0 -> Ok Field_Captain
          | 1 -> Ok Field_Chief_petty_officer
          | _ -> Serde.De.Error.invalid_variant_index ~idx

        let _ = visit_int

        let visit_string str =
          match str with
          | "Captain" -> Ok Field_Captain
          | "Chief_petty_officer" -> Ok Field_Chief_petty_officer
          | _ -> Serde.De.Error.unknown_variant str

        let _ = visit_string
      end)

      module Variant_visitor_for_Captain :
        Serde.De.Visitor.Intf with type value = rank and type tag = unit =
      Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = rank
        type tag = unit
      end)

      module Variant_visitor_for_Chief_petty_officer :
        Serde.De.Visitor.Intf with type value = rank and type tag = unit =
      Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = rank
        type tag = unit
      end)

      module Visitor_for_rank :
        Serde.De.Visitor.Intf with type value = rank and type tag = variants =
      Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = rank
        type tag = variants

        let visit_variant va =
          let* tag = Serde.De.Variant_access.tag va in
          match tag with
          | Field_Captain ->
              let* () = Serde.De.Variant_access.unit_variant va in
              Ok Captain
          | Field_Chief_petty_officer ->
              let* () = Serde.De.Variant_access.unit_variant va in
              Ok Chief_petty_officer

        let _ = visit_variant
      end)

      let deserialize_rank (type de_state) :
          (module Serde.De.Deserializer with type state = de_state) ->
          (rank, 'error Serde.De.de_error) result =
       fun (module De) ->
        Serde.De.deserialize_variant ~name ~variants
          (module De)
          (module Visitor_for_rank)
          (module Tag_visitor_for_rank)

      let _ = deserialize_rank
    end

    include Serde_deserialize_rank
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  type t = { name : string; rank : rank } [@@deriving serializer, deserializer]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    let serialize_t t =
      let ( let* ) = Result.bind in
      let* () = Ok () in
      let { name = f_0; rank = f_1 } = t in
      let* f_0 = Ser.serialize_string f_0 in
      let* f_1 = serialize_rank f_1 in
      let fields = [ ("name", f_0); ("rank", f_1) ] in
      Ser.serialize_record ~typename:"t" ~size:2 ~fields
      [@@ocaml.doc " Serialize a value of this type into Serde.data "]

    let _ = serialize_t

    module Serde_deserialize_t = struct
      let name = "t"
      let _ = name
      let fields = [ "name"; "rank" ]
      let _ = fields

      type nonrec fields = Field_name | Field_rank

      module Field_visitor_for_t = Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = fields
        type tag = unit

        let visit_int idx =
          match idx with
          | 0 -> Ok Field_name
          | 1 -> Ok Field_rank
          | _ -> Serde.De.Error.invalid_variant_index ~idx

        let _ = visit_int

        let visit_string str =
          match str with
          | "name" -> Ok Field_name
          | "rank" -> Ok Field_rank
          | _ -> Serde.De.Error.unknown_variant str

        let _ = visit_string
      end)

      module Visitor_for_t = Serde.De.Visitor.Make (struct
        include Serde.De.Visitor.Unimplemented

        type value = t
        type tag = fields

        let visit_seq :
            type de_state.
            (module Serde.De.Visitor.Intf with type value = value) ->
            (module Serde.De.Deserializer with type state = de_state) ->
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
                Serde.De.Error.message (Printf.sprintf "t needs 2 argument")
            | Some f0 -> Ok f0
          in
          let* f_1 =
            let deser_element () = deserialize_rank (module De) in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None ->
                Serde.De.Error.message (Printf.sprintf "t needs 2 argument")
            | Some f0 -> Ok f0
          in
          Ok { name = f_0; rank = f_1 }

        let _ = visit_seq

        let visit_map :
            type de_state.
            value Serde.De.Visitor.t ->
            de_state Serde.De.Deserializer.t ->
            (value, 'error) Serde.De.Map_access.t ->
            (value, 'error Serde.De.Error.de_error) result =
         fun (module Self) (module De) map_access ->
          let f_0 = ref None in
          let f_1 = ref None in
          let deser_key () =
            Serde.De.deserialize_identifier
              (module De)
              (module Field_visitor_for_t)
          in
          let rec fill () =
            let* key = Serde.De.Map_access.next_key map_access ~deser_key in
            match key with
            | None -> Ok ()
            | Some f ->
                let* () =
                  match f with
                  | Field_name ->
                      let* value =
                        Serde.De.Map_access.next_value map_access
                          ~deser_value:(fun () ->
                            Serde.De.deserialize_string
                              (module De)
                              (module Serde.De.Impls.String_visitor))
                      in
                      Ok (f_0 := value)
                  | Field_rank ->
                      let* value =
                        Serde.De.Map_access.next_value map_access
                          ~deser_value:(fun () -> deserialize_rank (module De))
                      in
                      Ok (f_1 := value)
                in
                fill ()
          in
          let* () = fill () in
          let* f_0 =
            match !f_0 with
            | Some value -> Ok value
            | None -> Serde.De.Error.missing_field "name"
          in
          let* f_1 =
            match !f_1 with
            | Some value -> Ok value
            | None -> Serde.De.Error.missing_field "rank"
          in
          Ok { name = f_0; rank = f_1 }

        let _ = visit_map
      end)

      let deserialize_t (type de_state) :
          (module Serde.De.Deserializer with type state = de_state) ->
          (t, 'error Serde.De.de_error) result =
       fun (module De) ->
        Serde.De.deserialize_record ~name ~fields
          (module De)
          (module Visitor_for_t)
          (module Field_visitor_for_t)

      let _ = deserialize_t
    end

    include Serde_deserialize_t
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
end
