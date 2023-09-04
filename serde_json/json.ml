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

module Parser = struct
  type t = { yojson : Yojson.lexer_state; lexbuf : Lexing.lexbuf }

  let debug t =
    Printf.printf "buff:\n`%s`\n" (Bytes.unsafe_to_string t.lexbuf.lex_buffer)

  let of_string ~string =
    { yojson = Yojson.init_lexer (); lexbuf = Lexing.from_string string }

  let _run fn =
    match fn () with
    | exception Yojson.Json_error reason -> Serde.De.Error.message reason
    | value -> Ok value

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
        try Yojson.Safe.read_object_end lexbuf with Yojson.End_of_object -> ())

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
