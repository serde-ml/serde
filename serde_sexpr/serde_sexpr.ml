open Serde
module S = Sexplib.Sexp

let ( let* ) = Result.bind

module Serializer : Ser.Intf with type output = S.t = Ser.Make (struct
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
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~size:_ ~elements =
    let* parts = Ser.map elements in
    Ok (S.List parts)

  and serialize_unit_variant _ser _output ~type_name:_ ~variant_name
      ~variant_index:_ =
    Ok (S.Atom (":" ^ variant_name))

  and serialize_tuple_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name:_ ~variant_index:_ ~variant_name ~variant_size:_
      ~fields =
    let* fields = Ser.map fields in

    Ok (S.List [ S.Atom (":" ^ variant_name); S.List fields ])

  and serialize_record_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name:_ ~variant_index:_ ~variant_name ~variant_size:_
      ~fields =
    let* fields = Ser.map_field fields in

    let fields = fields |> List.map (fun (_name, sexpr) -> sexpr) in

    Ok (S.List [ S.Atom (":" ^ variant_name); S.List fields ])

  and serialize_record
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~record_size:_ ~fields =
    let* fields = Ser.map_field fields in
    let fields =
      fields
      |> List.map (fun (name, sexpr) ->
             S.List [ S.Atom ":name"; S.Atom name; S.Atom ":value"; sexpr ])
    in
    Ok (S.List [ S.Atom type_name; S.List fields ])
end)

module Deserializer = Serde.De.Make (struct
  open Serde.De
  include Unimplemented

  let _read_keyword (module Reader : Reader.Instance) str =
    let rec aux acc chars =
      match chars with
      | [] -> Ok acc
      | c :: cs -> (
          match Reader.peek () with
          | Some c' when c == c' ->
              let _ = Reader.drop () in
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
      (module Deserializer) ->
      (module Reader.Instance) ->
      (module Visitor.Intf with type value = value) ->
      (value, 'error de_error) result =
   fun _ (module Reader) (module V) ->
    match Reader.peek () with
    | Some '0' .. '9' -> (
        let rec acc_int acc =
          match Reader.peek () with
          | Some (('0' .. '9' | '.') as c) ->
              Reader.drop ();
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
      (module Deserializer) ->
      (module Reader.Instance) ->
      (module Visitor.Intf with type value = value) ->
      (value, 'error de_error) result =
   fun _ (module Reader) (module V) ->
    match Reader.peek () with
    | Some 't' ->
        let* _ = _read_keyword (module Reader) "true" in
        V.visit_bool true
    | Some 'f' ->
        let* _ = _read_keyword (module Reader) "false" in
        V.visit_bool false
    | Some c ->
        Error.message
          (Printf.sprintf
             "expected bool to be 'true' or 'false', but instead found: %c" c)
    | None -> Error.message "end of stream!"

  let deserialize_string :
      type value.
      (module Deserializer) ->
      (module Reader.Instance) ->
      (module Visitor.Intf with type value = value) ->
      (value, 'error de_error) result =
   fun _ (module Reader) (module V) ->
    match Reader.peek () with
    | Some '"' ->
        Reader.drop ();
        let rec acc_str acc =
          match Reader.peek () with
          | Some '"' ->
              Reader.drop ();
              Ok acc
          | Some c ->
              Reader.drop ();
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

  let deserialize_seq :
      type value.
      (module Deserializer) ->
      (module Reader.Instance) ->
      (module Visitor.Intf with type value = value) ->
      (value, 'error de_error) result =
   fun (module Self) (module Reader) (module V) ->
    Reader.skip_whitespace ();
    match Reader.peek () with
    | Some '(' -> (
        Reader.drop ();
        let seq_access : (value, 'error) Sequence_access.t =
          {
            next_element =
              (fun ~deser_element ->
                Reader.skip_whitespace ();
                match Reader.peek () with
                | Some ')' ->
                    Reader.drop ();
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
        Reader.skip_whitespace ();
        match Reader.peek () with
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
      (module Deserializer) ->
      (module Reader.Instance) ->
      (module Visitor.Intf with type value = value) ->
      (value, 'error de_error) result =
   fun _ (module Reader) (module V) ->
    Reader.skip_whitespace ();
    match Reader.peek () with
    | Some ':' ->
        Reader.drop ();
        let rec acc_str acc =
          match Reader.peek () with
          | Some (('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_') as c) ->
              Reader.drop ();
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
      (module Deserializer) ->
      (module Reader.Instance) ->
      (module Visitor.Intf with type value = value and type tag = tag) ->
      (module Visitor.Intf with type value = tag) ->
      name:string ->
      variants:string list ->
      (value, 'error de_error) result =
   fun (module Self) (module Reader) (module Val) (module Tag) ~name:_
       ~variants:_ ->
    Reader.skip_whitespace ();
    match Reader.peek () with
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
                Reader.skip_whitespace ();
                Ok id);
            unit_variant =
              (fun () ->
                Reader.skip_whitespace ();
                Ok ());
            tuple_variant =
              (fun _ ->
                Error.message (Printf.sprintf "unexpected tuple variant"));
            record_variant =
              (fun _ ->
                Error.message (Printf.sprintf "unexpected record variant"));
          }
        in
        let* value = Val.visit_variant unit_variant_access in
        Reader.skip_whitespace ();
        Ok value
    (* NOTE(@ostera): if we find a ( then we are dealing with a tuple or record variant.
    *)
    | Some '(' -> (
        Reader.drop ();
        let variant_access : (tag, value, 'error) Variant_access.t =
          {
            tag =
              (fun () ->
                let* id =
                  Serde.De.deserialize_identifier (module Self) (module Tag)
                in
                Reader.skip_whitespace ();
                Ok id);
            unit_variant =
              (fun () ->
                Error.message (Printf.sprintf "unexpected unit variant"));
            tuple_variant =
              (fun v ->
                let* tuple = Serde.De.deserialize_seq (module Self) v in
                Reader.skip_whitespace ();
                Ok tuple);
            record_variant =
              (fun v ->
                let* record = Serde.De.deserialize_seq (module Self) v in
                Reader.skip_whitespace ();
                Ok record);
          }
        in
        let* value = Val.visit_variant variant_access in
        Reader.skip_whitespace ();
        match Reader.peek () with
        | Some ')' -> Ok value
        | Some _ ->
            Error.message
              "expected closed parenthesis when parsing tuple/record variant"
        | None -> Error.message "end of stream!")
    | Some c -> Error.message ("expected ( or : but found " ^ String.make 1 c)
    | None -> Error.message "end of stream!"

  let deserialize_record :
      type value tag.
      (module Deserializer) ->
      (module Reader.Instance) ->
      (module Visitor.Intf with type value = value and type tag = tag) ->
      (module Visitor.Intf with type value = tag) ->
      name:string ->
      fields:string list ->
      (value, 'error de_error) result =
   fun (module Self) (module Reader) (module Val) (module Field) ~name ~fields:_ ->
    Reader.skip_whitespace ();
    match Reader.peek () with
    | Some '(' -> (
        Reader.drop ();
        let* _ = _read_keyword (module Reader) (":" ^ name) in
        let seq_access : (value, 'error) Sequence_access.t =
          {
            next_element =
              (fun ~deser_element ->
                Reader.skip_whitespace ();
                match Reader.peek () with
                | Some ')' ->
                    Reader.drop ();
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
        Reader.skip_whitespace ();
        match Reader.peek () with
        | Some ')' -> Ok value
        | Some _ ->
            Error.message "expected closed parenthesis when parsing a record"
        | None -> Error.message "end of stream!")
    | Some c -> Error.message ("expected ( but found " ^ String.make 1 c)
    | None -> Error.message "end of stream!"
end)

let to_string_pretty fn t =
  let* t = fn t in
  let* sexp = Serde.serialize (module Serializer) t in
  Ok (Sexplib.Sexp.to_string_hum sexp)

let of_string de_fn (str : string) =
  let r = Serde.De.Reader.from_string str in
  let d = Deserializer.make r in
  de_fn d
