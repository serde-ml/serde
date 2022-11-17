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

  and serialize_unit_variant _ser _output ~type_name ~variant_name
      ~variant_index:_ =
    Ok (S.Atom (type_name ^ "#" ^ variant_name))

  and serialize_tuple_variant
      (module Ser : Ser.Mapper with type output = output and type error = error)
      _output ~type_name ~variant_index:_ ~variant_name ~variant_size:_ ~fields
      =
    let* fields = Ser.map fields in

    Ok (S.List [ S.Atom (type_name ^ "#" ^ variant_name); S.List fields ])

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

  let deserialize_variant :
      type value tag.
      (module Intf) ->
      (module Visitor.Intf with type value = value) ->
      (module Visitor.Intf with type value = tag) ->
      (module Reader.Instance) ->
      name:string ->
      variants:string list ->
      (value, 'error de_error) result =
   fun (module Self) (module Val) (module Tag) (module Reader) ~name:_
       ~variants:_ ->
    Reader.skip_whitespace ();
    match Reader.peek () with
    | Some '(' -> (
        Reader.drop ();

        (* NOTE(@ostera): we will create a new Variant_access module here to facilitate
            parsing specific variants by looking first at the Tag, getting a typed value
            that we can match on, and only if we match on something we care about, we
            will proceed to deserialize the value based on how we know it was serialized.
        *)
        let module Variant_access : Variant_access_intf = struct
          type tag = Tag.value
          type value = Val.value

          let tag () =
            let* id =
              Serde.De.deserialize_identifier (module Self) (module Tag)
            in
            Reader.skip_whitespace ();
            Ok id

          let unit_variant () =
            let* id = Serde.De.deserialize_unit (module Self) (module Val) in
            Reader.skip_whitespace ();
            Ok (Some id)

          let tuple_variant () =
            let* id = Serde.De.deserialize_seq (module Self) (module Val) in
            Reader.skip_whitespace ();
            Ok (Some id)

          let record_variant () =
            let* id = Serde.De.deserialize_record (module Self) (module Val) in
            Reader.skip_whitespace ();
            Ok (Some id)
        end in
        (* TODO(@ostera): reconciliate error types here to use `let*` *)
        let value =
          Val.visit_variant (module Variant_access) |> Result.get_ok
        in
        Reader.skip_whitespace ();
        match Reader.peek () with
        | Some ')' -> Ok value
        | Some _ -> Error.message "expected closed parenthesis"
        | None -> Error.message "end of stream!")
    | Some _ -> Error.message "expected open parenthesis"
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
