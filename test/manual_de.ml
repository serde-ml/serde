let ( let* ) = Result.bind

type local = bool

module Serde_deserialize_local : sig
  type error = unit

  val deserialize_local :
    (module Serde.De.Intf) -> (local, error Serde.De.de_error) result
end = struct
  type error = unit

  let deserialize_local (module De : Serde.De.Intf) =
    Serde.De.deserialize_bool (module De) (module Serde.De.Impls.Bool_visitor)
end

module Other = struct
  type other = int

  module Serde_deserialize_other : sig
    type error = unit

    val deserialize_other :
      (module Serde.De.Intf) -> (other, error Serde.De.de_error) result
  end = struct
    type error = unit

    let deserialize_other (module De : Serde.De.Intf) =
      Serde.De.deserialize_int (module De) (module Serde.De.Impls.Int_visitor)
  end
end

type t =
  | Hello
  | World of string * Other.other
  | Salute of { name : string; role : string; clearance : int }

module Serde_deserialize_t : sig
  open Serde

  type error

  val deserialize_t : (module De.Intf) -> (t, error) result
end = struct
  module Serde_deserialize_t_salute = struct

    module Serde_deserialize_t_salute_field = struct
      module Visitor : De.Visitor.Intf = struct
        include De.Visitor.Unimplemented

        type visitor = unit
        type value = Name | Role | Clearance | Ignore__
        type error = unit

        let visit_int _visitor idx =
          match idx with
          | 0 -> Ok Name
          | 1 -> Ok Role
          | 2 -> Ok Clearance
          | _ -> Error (De.Invalid_variant_index { idx })

        let visit_string _visitor str =
          match str with
          | "name" -> Ok Name
          | "role" -> Ok Role
          | "clearance" -> Ok Clearance
          | _ -> Error (De.Unknown_variant { str })
      end

      let deserialize (module De : De.Intf) =
        Serde.De.deserialize_identifier (module De) (module Visitor)
    end

    module Visitor = struct
      include Serde.De.Visitor.Unimplemented

      let visit_seq _visitor (module De : De.Intf)
          (module Seq : Serde.De.Seq_access_intf) =
        let* f0 = Seq.next_element De.deserialize_string self in
        let* f1 = Seq.next_element De.deserialize_string self in
        let* f2 = Seq.next_element De.deserialize_int self in
        Ok (Salute { name = f0; role = f1; clearance = f2 })

      let visit_map _visitor (module De : De.Intf)
          (module Map : Serde.De.Map_access_intf) =
        let f0 : string option ref = ref None in
        let f1 : string option ref = ref None in
        let f2 : int option ref = ref None in

        let rec populate_fields () =
          match Map.next_key Serde_deserialize_t_salute_field.deserialize with
          | None -> Ok ()
          | Some Serde_deserialize_t_salute_field.Visitor.Name ->
              if Option.is_some f0 then Error (Serde.De.Duplicate_field "name")
              else
                let* v = Map.next_value De.deserialize_string () in
                f0 := Some v;
                populate_fields ()
          | Some Field_visitor.Role ->
              if Option.is_some f1 then Error (Serde.De.Duplicate_field "role")
              else
                let* v = Map.next_value De.deserialize_string () in
                f1 := Some v;
                populate_fields ()
          | Some Field_visitor.Clearance ->
              if Option.is_some f1 then
                Error (Serde.De.Duplicate_field "clearance")
              else
                let* v = Map.next_value De.deserialize_int () in
                f2 := Some v;
                populate_fields ()
          | Some _ -> populate_fields ()
        in

        let* () = populate_fields () in

        let* f0 =
          match f0.contents with
          | None -> Error (Serde.De.Missing_field "name")
          | Some v -> Ok v
        in
        let* f1 =
          match f1.contents with
          | None -> Error (Serde.De.Missing_field "role")
          | Some v -> Ok v
        in
        let* f2 =
          match f2.contents with
          | None -> Error (Serde.De.Missing_field "clearance")
          | Some v -> Ok v
        in

        Ok (Salute { name = f0; role = f1; clearance = f2 })
    end
  end

  let variants = [ "Hello"; "Nested_tuples"; "World"; "Salute" ]

  type error

  module Field_visitor = struct
    type field = Hello | Nested_tuples | World | Salute

    let of_int idx =
      match idx with
      | 0 -> Ok Hello
      | 1 -> Ok Nested_tuples
      | 2 -> Ok World
      | 3 -> Ok Salute
      | _ -> Error (De.Invalid_variant_index { idx })

    let of_string str =
      match str with
      | "Hello" -> Ok Hello
      | "Nested_tuples" -> Ok Nested_tuples
      | "World" -> Ok World
      | "Salute" -> Ok Salute
      | _ -> Error (De.Unknown_variant { str })
  end

  module Visitor = struct
    type t = unit

    include De.Visitor.Unexpected

    let visit_variant t (module De : De.Intf) =
      let* variant = De.read_variant_name () in
      match variant with
      | Field_visitor.Hello ->
          let* () = De.read_unit_variant () in
          Ok Hello
      | Field_visitor.Salute ->
          let* f0 = De.read_record_field De.read_string () in
          let* f1 = De.read_record_field De.read_string () in
          let* f2 = De.read_record_field De.read_int () in
          Ok (Salute { name = f0; role = f1; clearance = f2 })
      | Field_visitor.World ->
          let* f0 = De.read_tuple_element De.read_string () in
          let* f1 = De.read_tuple_element Other.deserialize_other () in
          Ok (World (f0, f1))
  end

  let deserialize_t (module De : De.Intf) =
    Serde.De.deserialize_variant
      (module De)
      variants
      (module Visitor)
      (module Field_visitor)
end

include Serde_deserialize_t
