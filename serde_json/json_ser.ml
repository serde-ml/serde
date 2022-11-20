open Serde

let ( let* ) = Result.bind

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

and serialize_unit_variant _ser _output ~type_name:_ ~variant_name
    ~variant_index:_ =
  Ok (Json.String variant_name)

and serialize_tuple_variant
    (module Ser : Ser.Mapper with type output = output and type error = error)
    _output ~type_name:_ ~variant_index:_ ~variant_name ~variant_size:_ ~fields
    =
  let* fields = Ser.map fields in

  Ok (Json.Object [ (variant_name, Json.Array fields) ])

and serialize_record_variant
    (module Ser : Ser.Mapper with type output = output and type error = error)
    _output ~type_name:_ ~variant_index:_ ~variant_name ~variant_size:_ ~fields
    =
  let* fields = Ser.map_field fields in
  Ok (Json.Object [ (variant_name, Json.Object fields) ])

and serialize_record
    (module Ser : Ser.Mapper with type output = output and type error = error)
    _output ~type_name:_ ~record_size:_ ~fields =
  let* fields = Ser.map_field fields in
  Ok (Json.Object fields)
