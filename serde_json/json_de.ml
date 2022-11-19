let ( let* ) = Result.bind

module Deserializer_factory : Serde.De.Factory with type state = Json.Parser.t =
Serde.De.Make (struct
  open Serde.De
  include Serde.De.Unimplemented

  type state = Json.Parser.t

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
    let length = ref 0 in
    let seq_access : (value, 'error) Sequence_access.t =
      {
        next_element =
          (fun ~deser_element ->
            Json.Parser.skip_space state;
            match Json.Parser.read_close_bracket state with
            | Ok () -> Ok None
            | _ ->
                let* () =
                  if !length > 0 then Json.Parser.read_comma state else Ok ()
                in
                let* value = deser_element () in
                length := !length + 1;
                Ok (Some value));
      }
    in
    let* value = V.visit_seq (module V) (module Self) seq_access in
    match Json.Parser.read_close_bracket state with
    | Ok () -> Ok value
    | _ -> Error.message "expected closed parenthesis to close a sequence"
end)

let of_string ~string =
  Deserializer_factory.make (Json.Parser.of_string ~string)
