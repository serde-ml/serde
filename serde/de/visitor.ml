module type Intf = Intf.Visitor_intf

type 'value t = (module Intf with type value = 'value)

type ('value, 'tag) with_tag =
  (module Intf with type value = 'value and type tag = 'tag)

module Unimplemented = struct
  let visit_bool : bool -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented "visit_bool"

  let visit_unit : unit -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented "visit_unit"

  let visit_char : char -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented "visit_char"

  let visit_int : int -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented "visit_int"

  let visit_float : float -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented "visit_float"

  let visit_string : string -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented "visit_string"

  let visit_seq :
        'state.
        'state t ->
        'state' Deserializer.t ->
        ('value, 'error) Sequence_access.t ->
        ('value, 'error Error.de_error) result =
   fun _ _ _ -> Error.unimplemented "visit_seq"

  let visit_variant :
      ('tag, 'value, 'error) Variant_access.t ->
      ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented "visit_variant"

  let visit_map :
        'state.
        'state Deserializer.t ->
        ('state, 'error) Map_access.t ->
        ('value, 'error Error.de_error) result =
   fun _ _ -> Error.unimplemented "visit_map"
end

module Make (B : Intf) = struct
  include B
end
