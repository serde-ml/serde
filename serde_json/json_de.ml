let ( let* ) = Result.bind

module Deserializer_factory : Serde.De.Factory with type state = Json.Parser.t =
Serde.De.Make (struct
  open Serde.De
  include Serde.De.Unimplemented

  type state = Json.Parser.t

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
      (module Deserializer with type state = state) ->
      (module Visitor.Intf with type value = value) ->
      (value, 'error de_error) result =
   fun state (module Self) (module V) ->
    Json.Parser.skip_space state;
    let* int = Json.Parser.read_int state in
    V.visit_int int

  let deserialize_seq :
      type value.
      state ->
      (module Deserializer with type state = state) ->
      (module Visitor.Intf with type value = value) ->
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
    match Json.Parser.read_close_bracket state with
    | Ok () -> Ok value
    | _ -> Error.message "expected closed bracket to close a sequence"
end)

let of_string ~string =
  Deserializer_factory.make (Json.Parser.of_string ~string)
