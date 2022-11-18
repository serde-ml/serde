open Serde

let ( let* ) = Result.bind

type dbg =
  | Int of int
  | Bool of bool
  | Float of float
  | String of string
  | Char of char
  | Tuple of dbg list
  | Unit
  | Variant_unit of (string * string)
  | Variant_tuple of (string * string * dbg list)
  | Variant_record of (string * string * (string * dbg) list)
  | Record of (string * (string * dbg) list)

module Serializer : Ser.Intf with type output = dbg = Ser.Make (struct
  type output = dbg
  type error = unit

  let initial_output () = Ok Unit

  let serialize_int _ser _output int = Ok (Int int)
  and serialize_bool _ser _output bool = Ok (Bool bool)
  and serialize_unit _ser _output () = Ok Unit
  and serialize_char _ser _output char = Ok (String (String.make 1 char))
  and serialize_float _ser _output float = Ok (Float float)
  and serialize_string _ser _output string = Ok (String string)

  and serialize_tuple
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~size:_ ~elements =
    let* parts = Ser.map elements in
    Ok (Tuple parts)

  and serialize_unit_variant _ser _output ~type_name ~variant_name
      ~variant_index:_ =
    Ok (Variant_unit (type_name, variant_name))

  and serialize_tuple_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~variant_index:_ ~variant_name ~variant_size:_ ~fields
      =
    let* fields = Ser.map fields in
    Ok (Variant_tuple (type_name, variant_name, fields))

  and serialize_record_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~variant_index:_ ~variant_name ~variant_size:_ ~fields
      =
    let* fields = Ser.map_field fields in
    Ok (Variant_record (type_name, variant_name, fields))

  and serialize_record
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~record_size:_ ~fields =
    let* fields = Ser.map_field fields in
    Ok (Record (type_name, fields))
end)

let rec pp ppf (t : dbg) =
  match t with
  | Int i -> Format.fprintf ppf "%i : int" i
  | Bool b -> Format.fprintf ppf "%b : bool" b
  | Float f -> Format.fprintf ppf "%f : float" f
  | String s -> Format.fprintf ppf "\"%s\" : string" s
  | Char c -> Format.fprintf ppf "%c : char" c
  | Unit -> Format.fprintf ppf "() : unit"
  | Tuple t ->
      Format.fprintf ppf "(";
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        (fun ppf value -> Format.fprintf ppf "%a" pp value)
        ppf t;
      Format.fprintf ppf ")"
  | Variant_unit (ty, var) -> Format.fprintf ppf "%s.%s : %s" ty var ty
  | Variant_tuple (ty, var, t) ->
      Format.fprintf ppf "%s.%s : %a" ty var pp (Tuple t)
  | Variant_record (ty, var, r) -> pp ppf (Record (ty ^ "." ^ var, r))
  | Record (ty, fields) ->
      Format.fprintf ppf "%s {\n" ty;
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf ";\n")
        (fun ppf (field, value) ->
          Format.fprintf ppf "  %s = %a" field pp value)
        ppf fields;
      Format.fprintf ppf "\n}"

let debug fn t =
  let* t = fn t in
  let* t = Serde.serialize (module Serializer) t in
  pp Format.std_formatter t;
  Ok ()
