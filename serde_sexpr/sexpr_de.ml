let ( let* ) = Result.bind

type t = { reader : Serde.De.Reader.t }

module Deserializer_factory : Serde.De.Factory with type state = t =
Serde.De.Make (struct
  open Serde.De
  include Serde.De.Unimplemented

  type state = t

  let _read_keyword state str =
    let rec aux acc chars =
      match chars with
      | [] -> Ok acc
      | c :: cs -> (
          match Reader.peek state.reader with
          | Some c' when c == c' ->
              Reader.drop state.reader;
              aux (acc ^ String.make 1 c) cs
          | Some c' ->
              Printf.sprintf
                "when reading string \"%s\", found character '%c' when \
                 expecting '%c', after reading: \"%s\""
                str c' c acc
              |> Error.message
          | None ->
              Printf.sprintf
                "when reading string \"%s\", unexpected end of string after \
                 reading: \"%s\""
                str acc
              |> Error.message)
    in
    aux "" (String.to_seq str |> List.of_seq)

  let deserialize_int :
      type value.
      state ->
      state Deserializer.t ->
      value Visitor.t ->
      (value, 'error de_error) result =
   fun state (module De) (module V) ->
    match Reader.peek state.reader with
    | Some '0' .. '9' -> (
        let rec acc_int acc =
          match Reader.peek state.reader with
          | Some (('0' .. '9' | '.') as c) ->
              Reader.drop state.reader;
              acc_int (acc ^ String.make 1 c)
          | Some _ -> Ok acc
          | None -> Ok acc
        in
        let* int = acc_int "" in
        match int_of_string_opt int with
        | Some int -> V.visit_int int
        | None ->
            Error.message
              (Printf.sprintf "could not parse %s into type int" int))
    | Some c ->
        Error.message
          (Printf.sprintf
             "expected int to begin with a number (0..9) but instead found: %c"
             c)
    | None -> Error.message "end of stream!"

  let deserialize_bool :
      type value.
      state ->
      state Deserializer.t ->
      value Visitor.t ->
      (value, 'error de_error) result =
   fun state (module De) (module V) ->
    match Reader.peek state.reader with
    | Some 't' ->
        let* _ = _read_keyword state "true" in
        V.visit_bool true
    | Some 'f' ->
        let* _ = _read_keyword state "false" in
        V.visit_bool false
    | Some c ->
        Error.message
          (Printf.sprintf
             "expected bool to be 'true' or 'false', but instead found: %c" c)
    | None -> Error.message "end of stream!"

  let deserialize_string :
      type value.
      state ->
      state Deserializer.t ->
      value Visitor.t ->
      (value, 'error de_error) result =
   fun state (module De) (module V) ->
    match Reader.peek state.reader with
    | Some '"' ->
        Reader.drop state.reader;
        let rec acc_str acc =
          match Reader.peek state.reader with
          | Some '"' ->
              Reader.drop state.reader;
              Ok acc
          | Some c ->
              Reader.drop state.reader;
              acc_str (acc ^ String.make 1 c)
          | None -> Error.message "unexpected end of string"
        in
        let* str = acc_str "" in
        V.visit_string str
    | Some c ->
        Error.message
          ("expected string to begin with \" (double-quotes), but instead \
            found: " ^ String.make 1 c)
    | None -> Error.message "end of stream!"

  let deserialize_null :
      type value.
      state -> state Deserializer.t -> (value option, 'error de_error) result =
   fun _ (module De) -> Error.unimplemented "unimplemented: deserialize_null"

  let deserialize_seq :
      type value.
      state ->
      state Deserializer.t ->
      value Visitor.t ->
      (value, 'error de_error) result =
   fun state (module Self) (module V) ->
    Reader.skip_whitespace state.reader;
    match Reader.peek state.reader with
    | Some '(' -> (
        Reader.drop state.reader;
        let seq_access : (value, 'error) Sequence_access.t =
          {
            next_element =
              (fun ~deser_element ->
                Reader.skip_whitespace state.reader;
                match Reader.peek state.reader with
                | Some ')' ->
                    Reader.drop state.reader;
                    Ok None
                | None ->
                    Error.message
                      "unexpected end of stream while parsing sequence"
                | _ ->
                    let* value = deser_element () in
                    Ok (Some value));
          }
        in
        let* value = V.visit_seq (module V) (module Self) seq_access in
        Reader.skip_whitespace state.reader;
        match Reader.peek state.reader with
        | Some ')' -> Ok value
        | Some _ ->
            Error.message "expected closed parenthesis to close a sequence"
        | None -> Error.message "unexpected end of stream!")
    | Some c ->
        Error.message
          ("expected ( to begin a sequence but found " ^ String.make 1 c)
    | None -> Error.message "unexpected end of stream!"

  let deserialize_identifier :
      type value.
      state ->
      state Deserializer.t ->
      value Visitor.t ->
      (value, 'error de_error) result =
   fun state (module De) (module V) ->
    Reader.skip_whitespace state.reader;
    match Reader.peek state.reader with
    | Some ':' ->
        Reader.drop state.reader;
        let rec acc_str acc =
          match Reader.peek state.reader with
          | Some (('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_') as c) ->
              Reader.drop state.reader;
              acc_str (acc ^ String.make 1 c)
          | Some _ | None -> Ok acc
        in
        let* str = acc_str "" in
        V.visit_string str
    | Some c ->
        Error.message
          ("expected identifier to begin with : (ex. :hello-world), but \
            instead found: " ^ String.make 1 c)
    | None -> Error.message "end of stream!"

  let deserialize_variant :
      type value tag.
      state ->
      state Deserializer.t ->
      (value, tag) Visitor.with_tag ->
      tag Visitor.t ->
      name:string ->
      variants:string list ->
      (value, 'error de_error) result =
   fun state (module Self) (module Val) (module Tag) ~name ~variants:_ ->
    Reader.skip_whitespace state.reader;
    match Reader.peek state.reader with
    (* NOTE(@ostera): if we find a : then we are dealing with a unit variant
       (a variant constructor that has no arguments). Since the `:` is part of
       the variant identifier, we can't drop it.
    *)
    | Some ':' ->
        let unit_variant_access : (tag, value, 'error) Variant_access.t =
          {
            tag =
              (fun () ->
                let* id =
                  Serde.De.deserialize_identifier (module Self) (module Tag)
                in
                Reader.skip_whitespace state.reader;
                Ok id);
            unit_variant =
              (fun () ->
                Reader.skip_whitespace state.reader;
                Ok ());
            tuple_variant =
              (fun _ ->
                Error.message (Printf.sprintf "unexpected tuple variant"));
            record_variant =
              (fun _ _ ~fields:_ ->
                Error.message (Printf.sprintf "unexpected record variant"));
          }
        in
        let* value = Val.visit_variant unit_variant_access in
        Reader.skip_whitespace state.reader;
        Ok value
    (* NOTE(@ostera): if we find a ( then we are dealing with a tuple or record variant.
    *)
    | Some '(' -> (
        Reader.drop state.reader;
        let variant_access : (tag, value, 'error) Variant_access.t =
          {
            tag =
              (fun () ->
                let* id =
                  Serde.De.deserialize_identifier (module Self) (module Tag)
                in
                Reader.skip_whitespace state.reader;
                Ok id);
            unit_variant =
              (fun () ->
                Error.message (Printf.sprintf "unexpected unit variant"));
            tuple_variant =
              (fun v ->
                let* tuple = Serde.De.deserialize_seq (module Self) v in
                Reader.skip_whitespace state.reader;
                Ok tuple);
            record_variant =
              (fun v field_v ~fields ->
                let* record =
                  Serde.De.deserialize_record
                    (module Self)
                    v field_v ~name ~fields
                in
                Reader.skip_whitespace state.reader;
                Ok record);
          }
        in
        let* value = Val.visit_variant variant_access in
        Reader.skip_whitespace state.reader;
        match Reader.peek state.reader with
        | Some ')' -> Ok value
        | Some _ ->
            Error.message
              "expected closed parenthesis when parsing tuple/record variant"
        | None -> Error.message "end of stream!")
    | Some c -> Error.message ("expected ( or : but found " ^ String.make 1 c)
    | None -> Error.message "end of stream!"

  let deserialize_record :
      type value tag.
      state ->
      state Deserializer.t ->
      (value, tag) Visitor.with_tag ->
      tag Visitor.t ->
      name:string ->
      fields:string list ->
      (value, 'error de_error) result =
   fun state (module Self) (module Val) (module Field) ~name:_ ~fields:_ ->
    Reader.skip_whitespace state.reader;
    match Reader.peek state.reader with
    | Some '(' -> (
        Reader.drop state.reader;
        let seq_access : (value, 'error) Sequence_access.t =
          {
            next_element =
              (fun ~deser_element ->
                Reader.skip_whitespace state.reader;
                match Reader.peek state.reader with
                | Some ')' ->
                    Reader.drop state.reader;
                    Ok None
                | None ->
                    Error.message
                      "unexpected end of stream while parsing sequence"
                | _ ->
                    let* value = deser_element () in
                    Ok (Some value));
          }
        in
        let* value = Val.visit_seq (module Val) (module Self) seq_access in
        Reader.skip_whitespace state.reader;
        match Reader.peek state.reader with
        | Some ')' -> Ok value
        | Some _ ->
            Error.message "expected closed parenthesis when parsing a record"
        | None -> Error.message "end of stream!")
    | Some c -> Error.message ("expected ( but found " ^ String.make 1 c)
    | None -> Error.message "end of stream!"
end)

let of_string str =
  let state = { reader = Serde.De.Reader.from_string str } in
  Deserializer_factory.make state
