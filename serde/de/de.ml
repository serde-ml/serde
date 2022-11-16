include Error

module type Intf = Intf.Intf
module type Map_access_intf = Intf.Map_access_intf
module type Seq_access_intf = Intf.Seq_access_intf
module type Variant_access_intf = Intf.Variant_access_intf

module Impls = Impls
module Visitor = Visitor
module Reader = Reader

module Make (B : Intf): sig 
  val make : (module Reader.Intf) -> (module Intf)
end = struct
  let make _reader = 
    let module D = struct
      type t = { _reader: (module Reader.Intf) }
      let _state = { _reader }
      include B
    end in
    (module D: Intf)
end

module Unimplemented = struct
  let deserialize_bool :
        'value 'error.
        (module Visitor.Intf with type error = 'error and type value = 'value) ->
        ('value, 'error Error.de_error) result =
   fun _ -> Error Unimplemented

  let deserialize_unit :
        'value 'error.
        (module Visitor.Intf with type error = 'error and type value = 'value) ->
        ('value, 'error Error.de_error) result =
   fun _ -> Error Unimplemented

  let deserialize_char :
        'value 'error.
        (module Visitor.Intf with type error = 'error and type value = 'value) ->
        ('value, 'error Error.de_error) result =
   fun _ -> Error Unimplemented

  let deserialize_int :
        'value 'error.
        (module Visitor.Intf with type error = 'error and type value = 'value) ->
        ('value, 'error Error.de_error) result =
   fun _ -> Error Unimplemented

  let deserialize_float :
        'value 'error.
        (module Visitor.Intf with type error = 'error and type value = 'value) ->
        ('value, 'error Error.de_error) result =
   fun _ -> Error Unimplemented

  let deserialize_string :
        'value 'error.
        (module Visitor.Intf with type error = 'error and type value = 'value) ->
        ('value, 'error Error.de_error) result =
   fun _ -> Error Unimplemented

  let deserialize_tuple :
        'value 'error.
        (module Visitor.Intf with type error = 'error and type value = 'value) ->
        ('value, 'error Error.de_error) result =
   fun _ -> Error Unimplemented

  let deserialize_variant :
        'value 'variant 'error.
        (module Visitor.Intf with type error = 'error and type value = 'value) ->
        (module Visitor.Intf with type error = 'error and type value = 'variant) ->
        name:string ->
        variants:string list ->
        ('value, 'error Error.de_error) result =
   fun _ _ ~name:_ ~variants:_ -> Error Unimplemented

  let deserialize_record :
        'value 'error.
        (module Visitor.Intf with type error = 'error and type value = 'value) ->
        ('value, 'error Error.de_error) result =
   fun _ -> Error Unimplemented

  let deserialize_identifier :
        'value 'error.
        (module Visitor.Intf with type error = 'error and type value = 'value) ->
        ('value, 'error Error.de_error) result =
   fun _ -> Error Unimplemented
end

let deserialize_string :
    type value error.
    (module Intf) ->
    (module Visitor.Intf with type value = value and type error = error) ->
    (value, error de_error) result =
 fun (module De) (module V) -> De.deserialize_string (module V)

let deserialize_int :
    type value error.
    (module Intf) ->
    (module Visitor.Intf with type value = value and type error = error) ->
    (value, error de_error) result =
 fun (module De) (module V) -> De.deserialize_int (module V)

let deserialize_bool :
    type value error.
    (module Intf) ->
    (module Visitor.Intf with type value = value and type error = error) ->
    (value, error de_error) result =
 fun (module De) (module V) -> De.deserialize_bool (module V)

let deserialize_identifier :
    type value error.
    (module Intf) ->
    (module Visitor.Intf with type value = value and type error = error) ->
    (value, error de_error) result =
 fun (module De) (module V) -> De.deserialize_identifier (module V)

let deserialize_variant :
    type value variant error.
    (module Intf) ->
    (module Visitor.Intf with type value = value and type error = error) ->
    (module Visitor.Intf with type value = variant and type error = error) ->
    name:string ->
    variants:string list ->
    (value, error de_error) result =
 fun (module De) (module Value) (module Variant) ~name ~variants ->
  De.deserialize_variant (module Value) (module Variant) ~name ~variants

let deserialize :
    type value error.
    (module Visitor.Intf with type value = value and type error = error) ->
    (module Intf) ->
    (module Reader.Intf) ->
    (value, error de_error) result =
 fun (module Visitor) (module De) (module Reader) ->
  (*
  let ( let* ) = Result.bind in
  let visitor = visitor.make (intf.make (reader)) in
  visitor.do_stuff
  *)
  Obj.magic true
