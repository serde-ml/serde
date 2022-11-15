open Serde
module S = Sexplib.Sexp

let ( let* ) = Result.bind

module Serializer : Ser.Intf with type output = S.t = Ser.Make (struct
  type output = S.t
  type error = unit

  let initial_output () = Ok (S.List [])

  let serialize_int _ser _output v = Ok (S.Atom (v |> Int.to_string))
  and serialize_bool _ser _output bool = Ok (S.Atom (Bool.to_string bool))
  and serialize_unit _ser _output () = Ok (S.Atom "()")
  and serialize_char _ser _output char = Ok (S.Atom (String.make 1 char))
  and serialize_float _ser _output float = Ok (S.Atom (Float.to_string float))
  and serialize_string _ser _output string = Ok (S.Atom ("\"" ^ string ^ "\""))

  and serialize_tuple
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~size:_ ~elements =
    let* parts = Ser.map elements in
    Ok (S.List parts)

  and serialize_unit_variant _ser _output ~type_name ~variant_name
      ~variant_index:_ =
    Ok (S.Atom (type_name ^ "#" ^ variant_name))

  and serialize_tuple_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~variant_index:_ ~variant_name ~variant_size:_ ~fields
      =
    let* fields = Ser.map fields in

    Ok (S.List [ S.Atom (type_name ^ "#" ^ variant_name); S.List fields ])

  and serialize_record_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~variant_index:_ ~variant_name ~variant_size:_ ~fields
      =
    let* fields = Ser.map_field fields in

    let fields =
      fields
      |> List.map (fun (name, sexpr) ->
             S.List [ S.Atom ":name"; S.Atom name; S.Atom ":value"; sexpr ])
    in

    Ok (S.List [ S.Atom (type_name ^ "#" ^ variant_name); S.List fields ])

  and serialize_record
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~record_size:_ ~fields =
    let* fields = Ser.map_field fields in
    let fields =
      fields
      |> List.map (fun (name, sexpr) ->
             S.List [ S.Atom ":name"; S.Atom name; S.Atom ":value"; sexpr ])
    in
    Ok (S.List [ S.Atom type_name; S.List fields ])
end)

module Deserializer : De.Intf = struct
  include De.Unimplemented
end

let to_string_pretty t =
  let* sexp = Serde.serialize (module Serializer) t in
  Ok (Sexplib.Sexp.to_string_hum sexp)
