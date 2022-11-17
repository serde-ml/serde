module Unimplemented = struct
  let visit_bool : bool -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented

  let visit_unit : unit -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented

  let visit_char : char -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented

  let visit_int : int -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented

  let visit_float : float -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented

  let visit_string : string -> ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented

  let visit_seq :
      (module Intf.Deserializer_intf) ->
      (module Intf.Seq_access_intf) ->
      ('value, 'error Error.de_error) result =
   fun _ _ -> Error.unimplemented

  let visit_variant :
      (module Intf.Variant_access_intf) ->
      ('value, 'error Error.de_error) result =
   fun _ -> Error.unimplemented

  let visit_map :
      (module Intf.Deserializer_intf) ->
      (module Intf.Map_access_intf) ->
      ('value, 'error Error.de_error) result =
   fun _ _ -> Error.unimplemented
end

module type Intf = Intf.Visitor_intf

module Make (B : Intf) = struct
  include B
end
