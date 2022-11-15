open Serde

module Json = struct
  type t =
    | Null
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
    | Object of (string * t) list
    | Array of t list

  let rec to_yojson t: Yojson.Safe.t =
    match t with
    | Null -> `Null
    | Bool b -> `Bool b
    | Int i -> `Int i
    | Float f -> `Float f
    | String s -> `String s
    | Object o -> `Assoc (List.map (fun (k, v) -> (k, to_yojson v)) o)
    | Array a -> `List (List.map to_yojson a)
end

let ( let* ) = Result.bind

module Ser : Ser.Intf with type output = Json.t = Ser.Make (struct
  type output = Json.t
  type error = unit

  let initial_output () = Ok Json.Null

  let serialize_int _ser _output int = Ok (Json.Int int)
  and serialize_bool _ser _output bool = Ok (Json.Bool bool)
  and serialize_unit _ser _output () = Ok Json.Null
  and serialize_char _ser _output char = Ok (Json.String (String.make 1 char))
  and serialize_float _ser _output float = Ok (Json.Float float)
  and serialize_string _ser _output string = Ok (Json.String string)

  and serialize_tuple
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~size:_ ~elements =
    let* parts = Ser.map elements in
    Ok (Json.Array parts)

  and serialize_unit_variant _ser _output ~type_name ~variant_name
      ~variant_index:_ =
    Ok (Json.String (type_name ^ "#" ^ variant_name))

  and serialize_tuple_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~variant_index:_ ~variant_name ~variant_size:_ ~fields
      =
    let* fields = Ser.map fields in

    Ok (Json.Object [ (type_name ^ "#" ^ variant_name, Json.Array fields) ])

  and serialize_record_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~variant_index:_ ~variant_name ~variant_size:_ ~fields
      =
    let* fields = Ser.map_field fields in
    Ok (Json.Object [ (type_name ^ "#" ^ variant_name), Json.Object fields ])

  and serialize_record
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name:_ ~record_size:_ ~fields =
    let* fields = Ser.map_field fields in
    Ok (Json.Object fields)

end)

let to_string_pretty t =
  let* json = Serde.serialize (module Ser) t in
  let yojson = Json.to_yojson json in
  Ok (Yojson.Safe.pretty_to_string yojson)
