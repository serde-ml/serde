include Error
module Error = Error
module Impls = Impls
module Visitor = Visitor
module Reader = Reader
module Unimplemented = Unimplemented
module Variant_access = Variant_access
module Sequence_access = Sequence_access

module type Map_access_intf = Intf.Map_access_intf
module type Base = Intf.Deserializer_base_intf
module type Deserializer = Intf.Deserializer_intf

module type Factory = sig
  type state

  val make : state -> (module Deserializer with type state = state)
end

module Make (B : Base) : Factory with type state = B.state = struct
  type state = B.state

  let make state =
    let module D = struct
      include B

      let state = state
    end in
    (module D : Deserializer with type state = B.state)
end

(** boilerplace below is because we don't have modular implicits yet.
    
    it threads the two modules manually, and makes sure the all the types are
    escaping correctly.
*)

let deserialize_unit :
    type value state.
    (module Deserializer with type state = state) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) ->
  De.deserialize_unit De.state (module De) (module V)

let deserialize_string :
    type value state.
    (module Deserializer with type state = state) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) ->
  De.deserialize_string De.state (module De) (module V)

let deserialize_int :
    type value state.
    (module Deserializer with type state = state) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) ->
  De.deserialize_int De.state (module De) (module V)

let deserialize_bool :
    type value state.
    (module Deserializer with type state = state) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) ->
  De.deserialize_bool De.state (module De) (module V)

let deserialize_identifier :
    type value state.
    (module Deserializer with type state = state) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) ->
  De.deserialize_identifier De.state (module De) (module V)

let deserialize_record :
    type value field state.
    (module Deserializer with type state = state) ->
    (module Visitor.Intf with type value = value and type tag = field) ->
    (module Visitor.Intf with type value = field) ->
    name:string ->
    fields:string list ->
    (value, 'error de_error) result =
 fun (module De) (module Value) (module Field) ~name ~fields ->
  De.deserialize_record De.state
    (module De)
    (module Value)
    (module Field)
    ~name ~fields

let deserialize_seq :
    type value state.
    (module Deserializer with type state = state) ->
    (module Visitor.Intf with type value = value) ->
    (value, 'error de_error) result =
 fun (module De) (module V) ->
  De.deserialize_seq De.state (module De) (module V)

let deserialize_variant :
    type value tag state.
    (module Deserializer with type state = state) ->
    (module Visitor.Intf with type value = value and type tag = tag) ->
    (module Visitor.Intf with type value = tag) ->
    name:string ->
    variants:string list ->
    (value, 'error de_error) result =
 fun (module De) (module Value) (module Variant) ~name ~variants ->
  De.deserialize_variant De.state
    (module De)
    (module Value)
    (module Variant)
    ~name ~variants
