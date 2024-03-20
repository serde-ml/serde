open Serde

let ( let* ) = Result.bind

module Json = struct
  module Parser = struct
    type t = { yojson : Yojson.lexer_state; lexbuf : Lexing.lexbuf }

    let debug t =
      let buf = Bytes.unsafe_to_string t.lexbuf.lex_buffer in
      let padding = " " ^ String.make t.lexbuf.lex_curr_pos ' ' ^ "^" in
      Printf.printf "buff:\n`%s`\n%s\n" buf padding

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

    let read_int8 { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_int8 yojson lexbuf)

    let read_int { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_int yojson lexbuf)

    let read_int32 { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_int32 yojson lexbuf)

    let read_int64 { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_int64 yojson lexbuf)

    let read_float { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_number yojson lexbuf)

    let read_null_if_possible { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_null_if_possible yojson lexbuf)

    let read_null { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.read_null yojson lexbuf)

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

    let skip_any { yojson; lexbuf } =
      _run (fun () -> Yojson.Safe.skip_json yojson lexbuf) |> ignore;
      ()
  end
end

module Fmt = struct
  let write w buf = Rio.write w ~buf |> Result.map (fun _ -> ())
  let comma w = write w ","
  let begin_array w = write w "["
  let end_array w = write w "]"
  let begin_object w = write w "{"
  let end_object w = write w "}"
  let begin_object_key ?(first = false) w = if first then Ok () else write w ","
  let end_object_key _w = Ok ()
  let begin_object_value w = write w ":"
  let end_object_value _w = Ok ()
  let null w = write w "null"
end

module Serializer = struct
  type output = unit
  type kind = First | Rest
  type state = S : { fmt : 'w Rio.Writer.t; mutable kind : kind } -> state

  let nest (S { fmt; _ }) = S { fmt; kind = First }

  let serialize_bool _self (S { fmt; _ }) bool =
    Rio.write_all fmt ~buf:(Bool.to_string bool)

  let serialize_string _self (S { fmt; _ }) string =
    Rio.write_all fmt ~buf:(Format.sprintf "%S" string)

  let serialize_int8 _self (S { fmt; _ }) int =
    Rio.write_all fmt ~buf:(String.make 1 int)

  let serialize_int16 _self (S { fmt; _ }) int =
    Rio.write_all fmt ~buf:(Int.to_string int)

  let serialize_int31 _self (S { fmt; _ }) int =
    Rio.write_all fmt ~buf:(Int.to_string int)

  let serialize_int32 _self (S { fmt; _ }) int =
    Rio.write_all fmt ~buf:(Int32.to_string int)

  let serialize_int64 _self (S { fmt; _ }) int =
    Rio.write_all fmt ~buf:(Int64.to_string int)

  let serialize_float _self (S { fmt; _ }) float =
    Rio.write_all fmt ~buf:(Float.to_string float)

  let serialize_none _self (S { fmt; _ }) = Fmt.null fmt
  let serialize_some self _state value = Ser.serialize self value

  let serialize_sequence self (S ({ fmt; _ } as state)) ~size elements =
    state.kind <- First;
    let* () = Fmt.begin_array fmt in
    let* () = if size = 0 then Ok () else Ser.serialize self elements in
    Fmt.end_array fmt

  let serialize_element self (S s) elements =
    let* () = if s.kind = First then Ok () else Fmt.comma s.fmt in
    s.kind <- Rest;
    Ser.serialize self elements

  let serialize_unit_variant _self (S { fmt; _ }) ~var_type:_ ~cstr_idx:_
      ~cstr_name =
    Rio.write_all fmt ~buf:(Format.sprintf "%S" cstr_name)

  let serialize_tuple_variant self (S { fmt; _ }) ~var_type:_ ~cstr_idx:_
      ~cstr_name ~size values =
    let* () = Fmt.begin_object fmt in
    let* () = Fmt.begin_object_key ~first:true fmt in
    let* () = Rio.write_all fmt ~buf:(Format.sprintf "%S" cstr_name) in
    let* () = Fmt.end_object_key fmt in
    let* () = Fmt.begin_object_value fmt in
    let* () = Ser.serialize_sequence self size values in
    let* () = Fmt.end_object_value fmt in
    Fmt.end_object fmt

  let serialize_record_variant self (S { fmt; _ }) ~var_type:_ ~cstr_idx:_
      ~cstr_name ~size values =
    let* () = Fmt.begin_object fmt in
    let* () = Fmt.begin_object_key ~first:true fmt in
    let* () = Rio.write_all fmt ~buf:(Format.sprintf "%S" cstr_name) in
    let* () = Fmt.end_object_key fmt in
    let* () = Fmt.begin_object_value fmt in
    let* () = Ser.serialize_record self "" size values in
    let* () = Fmt.end_object_value fmt in
    Fmt.end_object fmt

  let serialize_newtype_variant self (S { fmt; _ }) ~var_type:_ ~cstr_idx:_
      ~cstr_name field =
    let* () = Fmt.begin_object fmt in
    let* () = Fmt.begin_object_key ~first:true fmt in
    let* () = Rio.write_all fmt ~buf:(Format.sprintf "%S" cstr_name) in
    let* () = Fmt.end_object_key fmt in
    let* () = Fmt.begin_object_value fmt in
    let* () = Ser.serialize self field in
    let* () = Fmt.end_object_value fmt in
    Fmt.end_object fmt

  let serialize_record self (S s) ~rec_type:_ ~size:_ values =
    let* () = Fmt.begin_object s.fmt in
    s.kind <- First;
    let* () = Ser.serialize self values in
    Fmt.end_object s.fmt

  let serialize_field self (S s) ~name values =
    let first = if s.kind = First then true else false in
    s.kind <- Rest;
    let* () = Fmt.begin_object_key ~first s.fmt in
    let* () = Rio.write_all s.fmt ~buf:(Format.sprintf "%S" name) in
    let* () = Fmt.end_object_key s.fmt in
    let* () = Fmt.begin_object_value s.fmt in
    let* () = Ser.serialize self values in
    let* () = Fmt.end_object_value s.fmt in
    Ok ()
end

module Deserializer = struct
  open Json

  type kind = First | Rest
  type state = { reader : Parser.t; mutable kind : kind }

  let nest { reader; _ } = { reader; kind = First }
  let deserialize_int8 _self state = Parser.read_int8 state.reader
  let deserialize_int16 _self state = Parser.read_int state.reader
  let deserialize_int31 _self state = Parser.read_int state.reader
  let deserialize_int32 _self state = Parser.read_int32 state.reader
  let deserialize_int64 _self state = Parser.read_int64 state.reader
  let deserialize_float _self state = Parser.read_float state.reader
  let deserialize_bool _self state = Parser.read_bool state.reader
  let deserialize_string _self state = Parser.read_string state.reader

  let deserialize_option self { reader; _ } de =
    match Parser.peek reader with
    | Some 'n' ->
        let* () = Parser.read_null reader in
        Ok None
    | _ ->
        let* v = De.deserialize self de in
        Ok (Some v)

  let deserialize_identifier self _state visitor =
    let* str = De.deserialize_string self in
    Visitor.visit_string self visitor str

  let deserialize_sequence self s ~size de =
    let* () = Parser.read_open_bracket s.reader in
    s.kind <- First;
    let* v = De.deserialize self (de ~size) in
    let* () = Parser.read_close_bracket s.reader in
    Ok v

  let deserialize_element self s de =
    match Parser.peek s.reader with
    | Some ']' -> Ok None
    | _ ->
        let* () =
          if s.kind = First then Ok () else Parser.read_comma s.reader
        in
        s.kind <- Rest;
        let* v = De.deserialize self de in
        Ok (Some v)

  let deserialize_unit_variant _self _state = Ok ()

  let deserialize_newtype_variant self { reader; _ } de =
    let* () = Parser.read_colon reader in
    De.deserialize self de

  let deserialize_tuple_variant self { reader; _ } ~size de =
    let* () = Parser.read_colon reader in
    De.deserialize_sequence self size de

  let deserialize_record_variant self { reader; _ } ~size de =
    let* () = Parser.read_colon reader in
    De.deserialize_record self "" size (de ~size)

  let deserialize_variant self { reader; _ } de ~name:_ ~variants:_ =
    Parser.skip_space reader;
    match Parser.peek reader with
    | Some '{' ->
        let* () = Parser.read_object_start reader in
        Parser.skip_space reader;
        let* value = De.deserialize self de in
        Parser.skip_space reader;
        let* () = Parser.read_object_end reader in
        Ok value
    | Some '"' -> De.deserialize self de
    | _ -> assert false

  let deserialize_record self s ~name:_ ~size:_ fields =
    Parser.skip_space s.reader;
    match Parser.peek s.reader with
    | Some '{' ->
        let* () = Parser.read_object_start s.reader in
        Parser.skip_space s.reader;
        s.kind <- First;
        let* value = De.deserialize self fields in
        Parser.skip_space s.reader;
        let* () = Parser.read_object_end s.reader in
        Ok value
    | Some c -> failwith (Format.sprintf "what: %c" c)
    | None -> failwith "unexpected eof"

  let deserialize_key self s visitor =
    Parser.skip_space s.reader;
    match Parser.peek s.reader with
    | Some '}' -> Ok None
    | _ ->
        let* () =
          if s.kind = First then Ok () else Parser.read_comma s.reader
        in
        s.kind <- Rest;
        Parser.skip_space s.reader;
        let* str = De.deserialize_string self in
        Parser.skip_space s.reader;
        let* key = Visitor.visit_string self visitor str in
        Parser.skip_space s.reader;
        let* () = Parser.read_colon s.reader in
        Parser.skip_space s.reader;
        Ok (Some key)

  let deserialize_field self s ~name:_ de =
    Parser.skip_space s.reader;
    De.deserialize self de

  let deserialize_ignored_any _self s =
    Parser.skip_space s.reader;
    match Parser.peek s.reader with
    | Some '}' -> Ok ()
    | Some ',' ->
        let* _ = Parser.read_comma s.reader in
        Parser.skip_space s.reader;
        let _ = Parser.skip_any s.reader in
        Parser.skip_space s.reader;
        Ok ()
    | Some _ ->
        Parser.skip_space s.reader;
        let _ = Parser.skip_any s.reader in
        Parser.skip_space s.reader;
        Ok ()
    | None -> failwith "unexpected eof"
end

let to_string ser value =
  let buf = Buffer.create 0 in
  let state = Serializer.S { fmt = Rio.Buffer.to_writer buf; kind = First } in
  let* () = Serde.serialize (module Serializer) state ser value in
  Ok (Buffer.to_bytes buf |> Bytes.unsafe_to_string)

let of_string de string =
  let state =
    Deserializer.{ reader = Json.Parser.of_string string; kind = First }
  in
  Serde.deserialize (module Deserializer) state de
