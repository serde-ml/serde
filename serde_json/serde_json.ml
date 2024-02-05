open Serde

let ( let* ) = Result.bind

module Json = struct
  type t =
    | Null
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
    | Object of (string * t) list
    | Array of t list

  let rec to_yojson t : Yojson.Safe.t =
    match t with
    | Null -> `Null
    | Bool b -> `Bool b
    | Int i -> `Int i
    | Float f -> `Float f
    | String s -> `String s
    | Object o -> `Assoc (List.map (fun (k, v) -> (k, to_yojson v)) o)
    | Array a -> `List (List.map to_yojson a)

  let pp_list pp_el fmt t =
    Format.fprintf fmt "[";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
      pp_el fmt t;
    Format.fprintf fmt "]"

  let rec pp fmt t =
    match t with
    | Null -> Format.fprintf fmt "Null"
    | Bool bool -> Format.fprintf fmt "(Bool %b)" bool
    | Int int -> Format.fprintf fmt "(Int %d)" int
    | Float float -> Format.fprintf fmt "(Float %f)" float
    | String string -> Format.fprintf fmt "(String %S)" string
    | Object fields ->
        Format.fprintf fmt "(Object %a)" (pp_list pp_field) fields
    | Array els -> Format.fprintf fmt "(Array %a)" (pp_list pp) els

  and pp_field fmt (name, t) = Format.fprintf fmt "(%S, %a)" name pp t

  module Parser = struct
    type t = { yojson : Yojson.lexer_state; lexbuf : Lexing.lexbuf }

    let debug t =
      Printf.printf "buff:\n`%s`\n" (Bytes.unsafe_to_string t.lexbuf.lex_buffer)

    let of_string string =
      { yojson = Yojson.init_lexer (); lexbuf = Lexing.from_string string }

    let _run fn = Ok (fn ())

    let peek { lexbuf; _ } =
      if lexbuf.lex_curr_pos < lexbuf.lex_buffer_len then
        Some (Bytes.unsafe_to_string lexbuf.lex_buffer).[lexbuf.lex_curr_pos]
      else None

    let read_bool { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_bool yojson lexbuf)

    let read_string { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_string yojson lexbuf)

    let read_int { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_int yojson lexbuf)

    let read_null_if_possible { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_null_if_possible yojson lexbuf)

    let read_object_start { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_lcurl yojson lexbuf)

    let read_field_sep { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_object_sep yojson lexbuf)

    let read_object_end { lexbuf; _ } =
      _run (fun () ->
          try Yojson.Safe.read_object_end lexbuf
          with Yojson.End_of_object -> ())

    let read_open_bracket { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_lbr yojson lexbuf)

    let read_close_bracket { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_rbr yojson lexbuf)

    let read_comma { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_comma yojson lexbuf)

    let read_colon { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_colon yojson lexbuf)

    let skip_space { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_space yojson lexbuf) |> ignore;
      ()
  end
end

module Json_ser : Serializer.Intf with type output = Json.t = struct
  open Ser

  type output = Json.t

  let serialize_bool _config bool = Json.Bool bool |> Result.ok
  let serialize_int _config int = Json.Int int |> Result.ok
  let serialize_string _config string = Json.String string |> Result.ok

  let serialize_record self _config { rec_type; rec_fields } =
    let* fields = Serializer.map_fields self rec_fields in
    Ok Json.(Object [ (rec_type, Object fields) ])

  let serialize_variant self _config { vcstr_name; vcstr_args; _ } =
    let* fields = Serializer.map self vcstr_args in
    Ok Json.(Object [ (vcstr_name, Array fields) ])

  let serialize_variant_record self _config { vrec_name; vrec_fields; _ } =
    let* fields = Serializer.map_fields self vrec_fields in
    Ok Json.(Object [ (vrec_name, Object fields) ])
end

let to_string ?config ser value =
  let* json = Serde.serialize ?config (module Json_ser) ser value in
  let yojson = Json.to_yojson json in
  Ok (Yojson.Safe.pretty_to_string yojson)

module Json_de = struct
  open De
  open Json

  type input = Parser.t

  let deserialize_int _config input =
    Parser.skip_space input;
    Parser.read_int input

  let deserialize_string _config input =
    Parser.skip_space input;
    Parser.read_string input

  let find_cstr_by_tag tag cstrs =
    List.find_opt
      (fun cstr ->
        match cstr with
        | Cstr_args { cstr_name = name; _ } | Cstr_unit { ucstr_name = name; _ }
          ->
            String.equal name tag)
      cstrs

  let deserialize_variant config self { var_name = _; var_cstrs } input =
    Parser.skip_space input;
    match Parser.peek input with
    | Some '{' -> (
        let* () = Parser.read_object_start input in
        Parser.skip_space input;
        let* tag = Parser.read_string input in
        Parser.skip_space input;
        let* () = Parser.read_colon input in
        Parser.skip_space input;
        let* () = Parser.read_open_bracket input in
        match find_cstr_by_tag tag var_cstrs with
        | Some (Cstr_args { cstr_fn; _ }) ->
            let ctx = (config, self, input) in
            let* result = Serde.Chain.execute cstr_fn ctx in
            Parser.skip_space input;
            let* () = Parser.read_close_bracket input in
            Parser.skip_space input;
            let* () = Parser.read_object_end input in
            Ok result
        | _ -> Error `invalid_field_type)
    | Some '"' -> (
        let* tag = Parser.read_string input in
        match find_cstr_by_tag tag var_cstrs with
        | Some (Cstr_unit { ucstr_val; _ }) -> ucstr_val
        | _ -> Error `invalid_field_type)
    | _ -> Error `unimplemented
end

let of_string ?config de value =
  Serde.deserialize ?config (module Json_de) de (Json.Parser.of_string value)
