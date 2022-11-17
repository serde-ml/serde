open Serde

let ( let* ) = Result.bind

type local = bool [@@deriving serializer]

module Serde_deserialize_local = struct
  let deserialize_local (module De : Serde.De.Intf) =
    Serde.De.deserialize_bool (module De) (module Serde.De.Impls.Bool_visitor)

  let _ = deserialize_local
end

include Serde_deserialize_local

module Other = struct
  type other = int [@@deriving serializer]

  module Serde_deserialize_other = struct
    let deserialize_other (module De : Serde.De.Intf) =
      Serde.De.deserialize_int (module De) (module Serde.De.Impls.Int_visitor)

    let _ = deserialize_other
  end
end

type t = Hello [@@deriving serializer]
(* | World of string * Other.other
   | Salute of { name : string; role : string; clearance : int }
*)

module Serde_deserialize_t = struct
  (*

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
*)

  let name = "t"
  let variants = [ "Hello"; "World"; "Salute" ]

  type error = unit
  type fields = Field_hello

  module Variant_visitor : Serde.De.Visitor.Intf with type value = fields =
  Serde.De.Visitor.Make (struct
    include Serde.De.Visitor.Unimplemented

    type value = fields
    type visitor = unit
    type error = unit

    let visit_int idx =
      match idx with
      | 0 -> Ok Field_hello
      | _ -> Serde.De.Error.invalid_variant_index ~idx

    let visit_str str =
      match str with
      | "Hello" -> Ok Field_hello
      | _ -> Serde.De.Error.unknown_variant str
  end)

  module Visitor = Serde.De.Visitor.Make (struct
    open Serde.De
    include Visitor.Unimplemented

    type visitor = bool
    type value = t

    let visit_variant
        (module Var : Variant_access_intf
          with type tag = fields
           and type value = value) =
      let* variant = Var.tag () in
      match variant with
      | Field_hello ->
          let* value = Var.unit_variant () in
          Ok (Option.get value)
    (* | Variant_visitor.Salute ->
           let* f0 = De.read_record_field De.read_string () in
           let* f1 = De.read_record_field De.read_string () in
           let* f2 = De.read_record_field De.read_int () in
           Ok (Salute { name = f0; role = f1; clearance = f2 })
       | Variant_visitor.World ->
           let* f0 = De.read_tuple_element De.read_string () in
           let* f1 = De.read_tuple_element Other.deserialize_other () in
           Ok (World (f0, f1))
    *)
  end)

  let deserialize_t (module De : Serde.De.Intf) =
    Serde.De.deserialize_variant ~name ~variants
      (module De)
      (module Visitor)
      (module Variant_visitor)
end

include Serde_deserialize_t

let _ =
  let t = Serde_sexpr.of_string deserialize_t "Hello" |> Result.get_ok in
  let sexpr = Serde_sexpr.to_string_pretty serialize_t t |> Result.get_ok in
  print_string sexpr;
  Ok ()
