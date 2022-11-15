include Error

module type Intf = Intf.Intf
module type Map_access_intf = Intf.Map_access_intf
module type Seq_access_intf = Intf.Seq_access_intf
module type Variant_access_intf = Intf.Variant_access_intf

module Impls = Impls
module Visitor = Visitor

module Unimplemented = struct
  let deserialize_bool _visitor = Error Unimplemented
  let deserialize_unit _visitor = Error Unimplemented
  let deserialize_char _visitor = Error Unimplemented
  let deserialize_int _visitor = Error Unimplemented
  let deserialize_float _visitor = Error Unimplemented
  let deserialize_string _visitor = Error Unimplemented
  let deserialize_tuple _visitor = Error Unimplemented
  let deserialize_unit_variant _visitor = Error Unimplemented
  let deserialize_tuple_variant _visitor = Error Unimplemented
  let deserialize_record_variant _visitor = Error Unimplemented
  let deserialize_record _visitor = Error Unimplemented
  let deserialize_identifier _visitor = Error Unimplemented
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
