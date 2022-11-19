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

  let of_string ~string =
    { yojson = Yojson.init_lexer (); lexbuf = Lexing.from_string string }

  let read_int { yojson; lexbuf } =
    match Yojson.Basic.read_int yojson lexbuf with
    | exception e ->
        Serde.De.Error.message
          (Printf.sprintf "could not read int: %s" (Printexc.to_string e))
    | value -> Ok value

  let read_open_bracket { yojson; lexbuf } =
    match Yojson.Basic.read_lbr yojson lexbuf with
    | exception e ->
        Serde.De.Error.message
          (Printf.sprintf "expected an open bracket: %s" (Printexc.to_string e))
    | value -> Ok value

  let read_close_bracket { yojson; lexbuf } =
    match Yojson.Basic.read_rbr yojson lexbuf with
    | exception e ->
        Serde.De.Error.message
          (Printf.sprintf "expected a close bracket: %s" (Printexc.to_string e))
    | value -> Ok value

  let read_comma { yojson; lexbuf } =
    match Yojson.Basic.read_comma yojson lexbuf with
    | exception e ->
        Serde.De.Error.message
          (Printf.sprintf "expected a close bracket: %s" (Printexc.to_string e))
    | value -> Ok value

  let skip_space { yojson; lexbuf } = Yojson.Basic.read_space yojson lexbuf
end
