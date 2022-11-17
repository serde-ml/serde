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
  and serialize_string _ser _output string = Ok (S.Atom ("\"" ^ string ^ "\""))

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
      _output ~type_name:_ ~variant_index:_ ~variant_name ~variant_size:_ ~fields
      =
    let* fields = Ser.map fields in

    Ok (S.List [ S.Atom (":" ^ variant_name); S.List fields ])

  and serialize_record_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~variant_index:_ ~variant_name ~variant_size:_ ~fields
      =
    let* fields = Ser.map_field fields in

    let fields =
      fields
      |> List.map (fun (name, sexpr) ->
             S.List [ S.Atom ":name"; S.Atom name; S.Atom ":value"; sexpr ])
    in

    Ok (S.List [ S.Atom (type_name ^ "#" ^ variant_name); S.List fields ])

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

  let _read_str (module Reader : Reader.Instance) str =
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
          | Some (('a' .. 'z' | 'A' .. 'Z' | '-' | '_') as c) ->
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
    (* NOTE(@ostera): if we find a :, then we are dealing with a unit variant *)
    | Some ':' ->
        let unit_variant_access : (tag, unit, 'error) Variant_access.t =
          Variant_access.
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
                (fun () ->
                  Error.message (Printf.sprintf "unexpected tuple variant"));
              record_variant =
                (fun () ->
                  Error.message (Printf.sprintf "unexpected record variant"));
            }
        in
        let* value = Val.visit_variant unit_variant_access in
        Reader.skip_whitespace ();
        Ok value
    | Some '(' -> (
        Reader.drop ();

        (* NOTE(@ostera): we will create a new Variant_access module here to facilitate
            parsing specific variants by looking first at the Tag, getting a typed value
            that we can match on, and only if we match on something we care about, we
            will proceed to deserialize the value based on how we know it was serialized.
        *)
        let variant_access : (tag, value, 'error) Variant_access.t =
          Variant_access.
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
                (fun () ->
                  let* id =
                    Serde.De.deserialize_seq (module Self) (module Val)
                  in
                  Reader.skip_whitespace ();
                  Ok (Some id));
              record_variant =
                (fun () ->
                  let* id =
                    Serde.De.deserialize_record (module Self) (module Val)
                  in
                  Reader.skip_whitespace ();
                  Ok (Some id));
            }
        in
        let* value = Val.visit_variant variant_access in
        Reader.skip_whitespace ();
        match Reader.peek () with
        | Some ')' -> Ok value
        | Some _ -> Error.message "expected closed parenthesis"
        | None -> Error.message "end of stream!")
    | Some c ->
        Error.message ("expected open parenthesis: found " ^ String.make 1 c)
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
