let ( let* ) = Result.bind

module Deserializer_factory : Serde.De.Factory with type state = Json.Parser.t =
Serde.De.Make (struct
  open Serde.De
  include Serde.De.Unimplemented

  type state = Json.Parser.t

  let deserialize_identifier :
      type value.
      state ->
      (module Deserializer with type state = state) ->
      (module Visitor.Intf with type value = value) ->
      (value, 'error de_error) result =
   fun state (module Self) (module V) ->
    Json.Parser.skip_space state;
    let* id = Json.Parser.read_string state in
    V.visit_string id

  let deserialize_bool :
      type value.
      state ->
      (module Deserializer with type state = state) ->
      (module Visitor.Intf with type value = value) ->
      (value, 'error de_error) result =
   fun state (module Self) (module V) ->
    Json.Parser.skip_space state;
    let* bool = Json.Parser.read_bool state in
    V.visit_bool bool

  let deserialize_int :
      type value.
      state ->
      state Deserializer.t ->
      value Visitor.t ->
      (value, 'error de_error) result =
   fun state (module Self) (module V) ->
    Json.Parser.skip_space state;
    let* int = Json.Parser.read_int state in
    V.visit_int int

  let deserialize_string :
      type value.
      state ->
      state Deserializer.t ->
      value Visitor.t ->
      (value, 'error de_error) result =
   fun state (module Self) (module V) ->
    Json.Parser.skip_space state;
    let* string = Json.Parser.read_string state in
    V.visit_string string

  let deserialize_option :
      type value.
      state -> state Deserializer.t -> (unit, 'error de_error) result =
   fun state (module Self) ->
    Json.Parser.skip_space state;
    let* is_null = Json.Parser.read_null_if_possible state in
    if is_null then Ok () else Error.message "not null"

  let deserialize_seq :
      type value.
      state ->
      state Deserializer.t ->
      value Visitor.t ->
      (value, 'error de_error) result =
   fun state (module Self) (module V) ->
    Json.Parser.skip_space state;
    let* () = Json.Parser.read_open_bracket state in
    let first_element = ref true in
    let seq_access : (value, 'error) Sequence_access.t =
      {
        next_element =
          (fun ~deser_element ->
            Json.Parser.skip_space state;
            let* () =
              if !first_element then (
                first_element := false;
                Ok ())
              else Json.Parser.read_comma state
            in
            let* value = deser_element () in
            Ok (Some value));
      }
    in
    let* value = V.visit_seq (module V) (module Self) seq_access in
    Json.Parser.skip_space state;
    let* () =
      match Json.Parser.peek state with
      | Some ',' -> Json.Parser.read_comma state
      | _ -> Ok ()
    in
    Json.Parser.skip_space state;
    match Json.Parser.read_close_bracket state with
    | Ok () -> Ok value
    | _ -> Error.message "expected closed bracket to close a sequence"

  let deserialize_record :
      type value field.
      state ->
      state Deserializer.t ->
      (value, field) Visitor.with_tag ->
      field Visitor.t ->
      name:string ->
      fields:string list ->
      (value, 'error de_error) result =
   fun state (module Self) (module Val) (module Field) ~name:_ ~fields:_ ->
    Json.Parser.skip_space state;
    match Json.Parser.peek state with
    | Some '[' -> Serde.De.deserialize_seq (module Self) (module Val)
    | Some '{' -> (
        let* () = Json.Parser.read_object_start state in
        let first_field = ref true in
        let map_access : (value, 'error) Map_access.t =
          {
            next_key =
              (fun ~deser_key ->
                Json.Parser.skip_space state;
                let* () =
                  if !first_field then (
                    first_field := false;
                    Ok ())
                  else
                    let* () =
                      match Json.Parser.peek state with
                      | Some ',' -> Json.Parser.read_comma state
                      | _ -> Ok ()
                    in
                    Json.Parser.skip_space state;
                    Ok ()
                in

                match Json.Parser.peek state with
                | None | Some '}' -> Ok None
                | _ ->
                    let* key = deser_key () in
                    Ok (Some key));
            next_value =
              (fun ~deser_value ->
                Json.Parser.skip_space state;
                let* () = Json.Parser.read_colon state in
                let* value = deser_value () in
                Ok (Some value));
          }
        in
        let* value = Val.visit_map (module Val) (module Self) map_access in
        Json.Parser.skip_space state;
        let* () =
          match Json.Parser.peek state with
          | Some ',' -> Json.Parser.read_comma state
          | _ -> Ok ()
        in
        Json.Parser.skip_space state;
        match Json.Parser.read_object_end state with
        | Ok () -> Ok value
        | _ -> Error.message "expected closed bracket to close a sequence")
    | Some c ->
        Error.message
          (Printf.sprintf
             "expected record to be serialized as an object (beginning with \
              '{') or as an array (beginning with '['), instead found \
              character %c"
             c)
    | None -> Error.message "unexpected end of stream!"

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
    Json.Parser.skip_space state;
    match Json.Parser.peek state with
    | Some '"' ->
        let unit_variant_access : (tag, value, 'error) Variant_access.t =
          {
            tag =
              (fun () ->
                Json.Parser.skip_space state;
                let* id =
                  Serde.De.deserialize_identifier (module Self) (module Tag)
                in
                Ok id);
            unit_variant =
              (fun () ->
                Json.Parser.skip_space state;
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
        Ok value
    | Some '{' -> (
        let* () = Json.Parser.read_object_start state in
        let variant_access : (tag, value, 'error) Variant_access.t =
          {
            tag =
              (fun () ->
                Json.Parser.skip_space state;
                let* id =
                  Serde.De.deserialize_identifier (module Self) (module Tag)
                in
                Ok id);
            unit_variant =
              (fun () ->
                Error.message (Printf.sprintf "unexpected unit variant"));
            tuple_variant =
              (fun v ->
                let* () = Json.Parser.read_colon state in
                Json.Parser.skip_space state;
                let* tuple = Serde.De.deserialize_seq (module Self) v in
                Ok tuple);
            record_variant =
              (fun v field_v ~fields ->
                let* () = Json.Parser.read_colon state in
                Json.Parser.skip_space state;
                let* record =
                  Serde.De.deserialize_record
                    (module Self)
                    v field_v ~name ~fields
                in
                Ok record);
          }
        in
        let* value = Val.visit_variant variant_access in
        Json.Parser.skip_space state;
        let* () =
          match Json.Parser.peek state with
          | Some ',' -> Json.Parser.read_comma state
          | _ -> Ok ()
        in
        Json.Parser.skip_space state;
        match Json.Parser.peek state with
        | Some '}' -> Ok value
        | _ -> Error.message "expected } to close a variant object")
    | Some c ->
        Error.message
          (Printf.sprintf
             "expected variant to be serialized as an object (beginning with \
              '{') instead found character %c"
             c)
    | None -> Error.message "unexpected end of stream!"
end)

let of_string ~string =
  Deserializer_factory.make (Json.Parser.of_string ~string)
