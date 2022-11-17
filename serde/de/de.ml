include Error

module Error = Error
module Impls = Impls
module Visitor = Visitor
module Reader = Reader
module Unimplemented = Unimplemented

module type Map_access_intf = Intf.Map_access_intf
module type Seq_access_intf = Intf.Seq_access_intf
module type Variant_access_intf = Intf.Variant_access_intf
module type Base = Intf.Deserializer_base_intf
module type Intf = Intf.Deserializer_intf

module type Factory = sig
  val make : (module Reader.Instance) -> (module Intf)
end

module Make (B : Base) : Factory = struct
  let make (module R : Reader.Instance) =
    let module D = struct
      module R = R
      include B
    end in
    (module D : Intf)
end

(** boilerplace below is because we don't have modular implicits yet.
    
    it threads the two modules manually, and makes sure the all the types are
    escaping correctly.
*)

let deserialize_unit :
    type value.
    (module Intf) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) -> De.deserialize_unit (module V)

let deserialize_string :
    type value.
    (module Intf) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) -> De.deserialize_string (module V)

let deserialize_int :
    type value.
    (module Intf) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) -> De.deserialize_int (module V)

let deserialize_bool :
    type value.
    (module Intf) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) -> De.deserialize_bool (module V)

let deserialize_identifier :
    type value.
    (module Intf) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) -> De.deserialize_identifier (module V)

let deserialize_record :
    type value.
    (module Intf) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) -> De.deserialize_record (module V)

let deserialize_seq :
    type value.
    (module Intf) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) -> De.deserialize_seq (module V)

let deserialize_variant :
    type value tag.
    (module Intf) ->
    (module Visitor.Intf with type value = value) ->
    (module Visitor.Intf with type value = tag) ->
    name:string ->
    variants:string list ->
    (value, 'error de_error) result =
 fun (module De) (module Value) (module Variant) ~name ~variants ->
  De.deserialize_variant
    (module De)
    (module Value)
    (module Variant)
    (module De.R : Reader.Instance)
    ~name ~variants
