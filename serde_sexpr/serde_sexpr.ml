open Serde

module Ser = Ser.Make (struct
  let ( let* ) = Result.bind

  type output = string
  type error = unit

  let initial_output () = Ok ""

  let serialize_int _ser output v = Ok (output ^ Int.to_string v)
  and serialize_bool _ser output bool = Ok (output ^ Bool.to_string bool)
  and serialize_unit _ser output () = Ok (output ^ "()")
  and serialize_char _ser output char = Ok (output ^ String.make 1 char)
  and serialize_float _ser output float = Ok (output ^ Float.to_string float)
  and serialize_string _ser output string = Ok (output ^ "\"" ^ string ^ "\"")

  and serialize_tuple
      (module Ser : Ser.Mapper with type output = output and type error = error)
      output ~size:_ ~elements =
    let* parts = Ser.map elements in
    let joined = parts |> String.concat " " in
    Ok (output ^ "(" ^ joined ^ ")")

  and serialize_unit_variant _ser output ~type_name ~variant_name
      ~variant_index:_ =
    Ok (output ^ type_name ^ "#" ^ variant_name)

  and serialize_tuple_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      output ~type_name ~variant_index:_ ~variant_name ~variant_size:_ ~fields =
    let* fields = Ser.map fields in
    Ok
      (output ^ "(" ^ type_name ^ "#" ^ variant_name ^ " ("
      ^ (fields |> String.concat " ")
      ^ ")" ^ ")")

  and serialize_record_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      output ~type_name ~variant_index:_ ~variant_name ~variant_size:_ ~fields =
    let* fields = Ser.map_field fields in
    Ok
      (output ^ "(" ^ type_name ^ "#" ^ variant_name ^ " ("
      ^ (fields |> List.map (fun (name, sexpr) -> "(:name " ^ name ^ " :value " ^ sexpr ^ ")") |> String.concat " ")
      ^ ")" ^ ")")

  and serialize_record
      (module Ser : Ser.Mapper with type output = output and type error = error)
      output ~type_name ~record_size:_ ~fields =
    let* fields = Ser.map_field fields in
    Ok
      (output ^ "(" ^ type_name ^ " ("
      ^ (fields |> List.map (fun (name, sexpr) -> "(:name " ^ name ^ " :value " ^ sexpr ^ ")") |> String.concat " ")
      ^ ")" ^ ")")
end)

let to_string t = Serde.serialize (module Ser) t
