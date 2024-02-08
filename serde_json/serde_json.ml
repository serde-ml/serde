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

module Fmt = struct
  let write w buf = Rio.write w ~buf |> Result.map (fun _ -> ())
  let begin_object w = write w "{"
  let end_object w = write w "}"
  let begin_object_key ?(first = false) w = if first then Ok () else write w ","
  let end_object_key _w = Ok ()
  let begin_object_value w = write w ":"
  let end_object_value _w = Ok ()
end

module Serializer = struct
  type output = unit
  type state = S : { fmt : 'w Rio.Writer.t } -> state

  let serialize_int _self (S { fmt }) int =
    Rio.write_all fmt ~buf:(Int.to_string int)

  let serialize_unit_variant _self (S { fmt }) ~var_type:_ ~cstr_idx:_
      ~cstr_name =
    Rio.write_all fmt ~buf:(Format.sprintf "%S" cstr_name)

  let serialize_newtype_variant self (S { fmt }) ~var_type:_ ~cstr_idx:_
      ~cstr_name field =
    let* () = Fmt.begin_object fmt in
    let* () = Fmt.begin_object_key ~first:true fmt in
    let* () = Rio.write_all fmt ~buf:(Format.sprintf "%S" cstr_name) in
    let* () = Fmt.end_object_key fmt in
    let* () = Fmt.begin_object_value fmt in
    let* () = Ser.serialize self field in
    let* () = Fmt.end_object_value fmt in
    Fmt.end_object fmt
end

module Deserializer = struct
  open Json

  type state = { reader : Parser.t }

  let deserialize_string self state visitor =
    let* str = Parser.read_string state.reader in
    Visitor.visit_string self visitor str

  let deserialize_identifier self _state visitor =
    De.deserialize_string self visitor

  let deserialize_unit_variant _self _state = Ok ()

  let deserialize_variant self state visitor ~name:_ ~variants:_ =
    Parser.skip_space state.reader;
    match Parser.peek state.reader with
    | Some '"' -> Visitor.visit_variant self visitor
    | _ -> assert false
end

let to_string ser value =
  let buf = Buffer.create 0 in
  let state = Serializer.S { fmt = Rio.Buffer.to_writer buf } in
  let* () = Serde.serialize (module Serializer) state ser value in
  Ok (Buffer.to_bytes buf |> Bytes.unsafe_to_string)

let of_string de string =
  let state = Deserializer.{ reader = Json.Parser.of_string string } in
  Serde.deserialize (module Deserializer) state de
