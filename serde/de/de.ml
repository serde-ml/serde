include Error
module Deserializer = Deserializer
module Error = Error
module Impls = Impls
module Reader = Reader
module Unimplemented = Unimplemented
module Visitor = Visitor

module Sequence_access = Sequence_access
(** Access modules *)

module Variant_access = Variant_access
module Map_access = Map_access

module type Base = Intf.Deserializer_base_intf
module type Deserializer = Intf.Deserializer_intf

module type Factory = sig
  type state

  val make : state -> state Deserializer.t
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
    state Deserializer.t -> value Visitor.t -> (value, 'error de_error) result =
 fun (module De) (module V) ->
  match De.deserialize_unit De.state (module De) (module V) with
  | exception e -> Error.unexpected_exception e
  | res -> res

let deserialize_string :
    type value state.
    state Deserializer.t -> value Visitor.t -> (value, 'error de_error) result =
 fun (module De) (module V) ->
  match De.deserialize_string De.state (module De) (module V) with
  | exception e -> Error.unexpected_exception e
  | res -> res

let deserialize_int :
    type value state.
    state Deserializer.t -> value Visitor.t -> (value, 'error de_error) result =
 fun (module De) (module V) ->
  match De.deserialize_int De.state (module De) (module V) with
  | exception e -> Error.unexpected_exception e
  | res -> res

let deserialize_bool :
    type value state.
    state Deserializer.t -> value Visitor.t -> (value, 'error de_error) result =
 fun (module De) (module V) ->
  match De.deserialize_bool De.state (module De) (module V) with
  | exception e -> Error.unexpected_exception e
  | res -> res

let deserialize_option :
    type value state.
    (state Deserializer.t -> value Visitor.t -> (value, 'error de_error) result) ->
    state Deserializer.t ->
    value Visitor.t ->
    (value option, 'error de_error) result =
 fun fn (module De) (module V) ->
  match De.deserialize_option De.state (module De) with
  | Ok _ -> Ok None
  | _ -> (
      match fn (module De) (module V) with
      | Ok x -> Ok (Some x)
      | Error _ as err -> err)

let deserialize_record_option :
    type state.
    ((module Deserializer with type state = state) ->
    ('a, 'error de_error) result) ->
    (module Deserializer with type state = state) ->
    ('a option, 'error de_error) result =
 fun fn (module De) ->
  match De.deserialize_option De.state (module De) with
  | Ok () -> Ok None
  | Error _err ->
      let (let*) = Result.bind in
      let* result = fn (module De) in
      Ok (Some result)

let deserialize_identifier :
    type value state.
    state Deserializer.t -> value Visitor.t -> (value, 'error de_error) result =
 fun (module De) (module V) ->
  match De.deserialize_identifier De.state (module De) (module V) with
  | exception e -> Error.unexpected_exception e
  | res -> res

let deserialize_record :
    type value field state.
    state Deserializer.t ->
    (value, field) Visitor.with_tag ->
    field Visitor.t ->
    name:string ->
    fields:string list ->
    (value, 'error de_error) result =
 fun (module De) (module Value) (module Field) ~name ~fields ->
  match
    De.deserialize_record De.state
      (module De)
      (module Value)
      (module Field)
      ~name ~fields
  with
  | exception e -> Error.unexpected_exception e
  | res -> res

let deserialize_seq :
    type value state.
    state Deserializer.t -> value Visitor.t -> (value, 'error de_error) result =
 fun (module De) (module V) ->
  match De.deserialize_seq De.state (module De) (module V) with
  | exception e -> Error.unexpected_exception e
  | res -> res

let deserialize_variant :
    type value tag state.
    state Deserializer.t ->
    (value, tag) Visitor.with_tag ->
    tag Visitor.t ->
    name:string ->
    variants:string list ->
    (value, 'error de_error) result =
 fun (module De) (module Value) (module Variant) ~name ~variants ->
  match
    De.deserialize_variant De.state
      (module De)
      (module Value)
      (module Variant)
      ~name ~variants
  with
  | exception e -> Error.unexpected_exception e
  | res -> res
