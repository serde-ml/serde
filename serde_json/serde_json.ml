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

module Serializer = struct
  type output = unit
  type state = S : { writer : 'w Rio.Writer.t } -> state

  let serialize_variant _self (S state) ~var_type:_ ~cstr_idx:_ ~cstr_name
      ~cstr_args =
    if cstr_args = 0 then
      Rio.write_all state.writer ~buf:(Format.sprintf "%S" cstr_name)
      |> Result.map_error (fun err -> `io_error err)
    else Ok ()
end

module Deserializer = struct
  open Json

  type state = { reader : Parser.t }

  let deserialize_string self state visitor = 
    let*str = Parser.read_string state.reader in
    Visitor.visit_string self visitor str

  let deserialize_identifier self _state visitor =
    De.deserialize_string self visitor

  let deserialize_variant self state visitor ~name:_ ~variants:_ =
    Parser.skip_space state.reader;
    match Parser.peek state.reader with
    | Some '"' -> Visitor.visit_variant self visitor
    | _ -> assert false
end

let to_string ser value =
  let buf = Buffer.create 0 in
  let state = Serializer.S { writer = Rio.Buffer.to_writer buf } in
  let* () = Serde.serialize (module Serializer) state ser value in
  Ok (Buffer.to_bytes buf |> Bytes.unsafe_to_string)

let of_string de string =
  let state = Deserializer.{ reader = Json.Parser.of_string string } in
  Serde.deserialize (module Deserializer) state de
