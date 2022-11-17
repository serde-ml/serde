open Error

let deserialize_any :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_bool :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_char :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_int :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_float :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_string :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_unit :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_tuple :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_variant :
      'value 'tag 'val_error 'tag_error.
      (module Intf.Deserializer_intf) ->
      (module Intf.Visitor_intf with type value = 'value) ->
      (module Intf.Visitor_intf with type value = 'tag) ->
      (module Reader.Instance) ->
      name:string ->
      variants:string list ->
      ( 'value,
        [> `Value_error of 'val_error | `Tag_error of 'tag_error ] de_error )
      result =
 fun _ _ _ _ ~name:_ ~variants:_ -> Error.unimplemented

let deserialize_unit_variant :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_tuple_variant :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_record_variant :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_record :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_seq :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_map :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented

let deserialize_identifier :
      'value 'error.
      (module Intf.Visitor_intf with type value = 'value) ->
      ('value, 'error de_error) result =
 fun _ -> Error.unimplemented
