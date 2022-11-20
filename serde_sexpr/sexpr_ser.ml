module S = Sexplib.Sexp

let ( let* ) = Result.bind

include Serde.Ser.Make (struct
  type output = S.t
  type error = unit

  let initial_output () = Ok (S.List [])

  let serialize_int _ser _output v = Ok (S.Atom (v |> Int.to_string))
  and serialize_bool _ser _output bool = Ok (S.Atom (Bool.to_string bool))
  and serialize_unit _ser _output () = Ok (S.Atom "()")
  and serialize_char _ser _output char = Ok (S.Atom (String.make 1 char))
  and serialize_float _ser _output float = Ok (S.Atom (Float.to_string float))

  and serialize_string _ser _output string =
    let string =
      if String.contains string ' ' then string else "\"" ^ string ^ "\""
    in
    Ok (S.Atom string)

  and serialize_tuple
      (module Ser : Serde.Ser.Mapper
        with type output = output
         and type error = error) _output ~size:_ ~elements =
    let* parts = Ser.map elements in
    Ok (S.List parts)

  and serialize_unit_variant _ser _output ~type_name:_ ~variant_name
      ~variant_index:_ =
    Ok (S.Atom (":" ^ variant_name))

  and serialize_tuple_variant
      (module Ser : Serde.Ser.Mapper
        with type output = output
         and type error = error) _output ~type_name:_ ~variant_index:_
      ~variant_name ~variant_size:_ ~fields =
    let* fields = Ser.map fields in

    Ok (S.List [ S.Atom (":" ^ variant_name); S.List fields ])

  and serialize_record_variant
      (module Ser : Serde.Ser.Mapper
        with type output = output
         and type error = error) _output ~type_name:_ ~variant_index:_
      ~variant_name ~variant_size:_ ~fields =
    let* fields = Ser.map_field fields in

    let fields = fields |> List.map (fun (_name, sexpr) -> sexpr) in

    Ok (S.List [ S.Atom (":" ^ variant_name); S.List fields ])

  and serialize_record
      (module Ser : Serde.Ser.Mapper
        with type output = output
         and type error = error) _output ~type_name:_ ~record_size:_ ~fields =
    let* fields = Ser.map_field fields in
    let fields = fields |> List.map (fun (_name, sexpr) -> sexpr) in
    Ok (S.List fields)
end)
