(* NOTE(@ostera): since standalone module signatures can't be made recursive,
   we use this old trick by making a temporary recursive module structure, that
   allows all these signatures to be co-recursive. Ugly, but does the job
*)
module rec Rec : sig
  module type DeserializerIntf = sig
    val deserialize_bool :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_unit :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_char :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_int :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_float :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_string :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_tuple :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_variant :
      'value 'variant 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      (module Rec.VisitorIntf with type error = 'error and type value = 'variant) ->
        name:string -> variants:(string list) ->
      ('value, 'error Error.de_error) result

    val deserialize_record :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_identifier :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result
  end

  module type VariantAccessIntf = sig
    val seed :
      (module Rec.VisitorIntf with type value = 'name) ->
      ('name, 'error Error.de_error) result

    val value :
      ((module Rec.DeserializerIntf) -> ('value, 'error Error.de_error) result) ->
      (module Rec.VisitorIntf) ->
      ('value option, 'error Error.de_error) result
  end

  module type SeqAccessIntf = sig
    val next_element :
      ((module Rec.DeserializerIntf) -> ('value, 'error Error.de_error) result) ->
      (module Rec.VisitorIntf) ->
      ('value option, 'error Error.de_error) result
  end

  module type MapAccessIntf = sig
    val next_key :
      ((module Rec.DeserializerIntf) -> ('key, 'error Error.de_error) result) ->
      (module Rec.VisitorIntf) ->
      ('key option, 'error Error.de_error) result

    val next_value :
      ((module Rec.DeserializerIntf) -> ('value, 'error Error.de_error) result) ->
      ('value option, 'error Error.de_error) result
  end

  module type VisitorIntf = sig
    type visitor
    type value
    type error

    val visit_bool : visitor -> bool -> (value, error Error.de_error) result
    val visit_unit : visitor -> unit -> (value, error Error.de_error) result
    val visit_char : visitor -> char -> (value, error Error.de_error) result
    val visit_int : visitor -> int -> (value, error Error.de_error) result
    val visit_float : visitor -> float -> (value, error Error.de_error) result
    val visit_string : visitor -> string -> (value, error Error.de_error) result

    val visit_seq :
      visitor ->
      (module Rec.DeserializerIntf) ->
      (module Rec.SeqAccessIntf) ->
      (value, error Error.de_error) result

    val visit_variant :
      visitor ->
      (module Rec.DeserializerIntf) ->
      (module Rec.VariantAccessIntf) ->
      (value, error Error.de_error) result

    val visit_map :
      visitor ->
      (module Rec.DeserializerIntf) ->
      (module Rec.MapAccessIntf) ->
      (value, error Error.de_error) result
  end
end = struct
  module type DeserializerIntf = sig
    val deserialize_bool :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_unit :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_char :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_int :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_float :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_string :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_tuple :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_variant :
      'value 'variant 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      (module Rec.VisitorIntf with type error = 'error and type value = 'variant) ->
        name:string -> variants:(string list) ->
      ('value, 'error Error.de_error) result

    val deserialize_record :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_identifier :
      'value 'error.
      (module Rec.VisitorIntf with type error = 'error and type value = 'value) ->
      ('value, 'error Error.de_error) result
  end

  module type VariantAccessIntf = sig
    val seed :
      (module Rec.VisitorIntf with type value = 'name) ->
      ('name, 'error Error.de_error) result

    val value :
      ((module Rec.DeserializerIntf) -> ('value, 'error Error.de_error) result) ->
      (module Rec.VisitorIntf) ->
      ('value option, 'error Error.de_error) result
  end

  module type SeqAccessIntf = sig
    val next_element :
      ((module Rec.DeserializerIntf) -> ('value, 'error Error.de_error) result) ->
      (module Rec.VisitorIntf) ->
      ('value option, 'error Error.de_error) result
  end

  module type MapAccessIntf = sig
    val next_key :
      ((module Rec.DeserializerIntf) -> ('key, 'error Error.de_error) result) ->
      (module Rec.VisitorIntf) ->
      ('key option, 'error Error.de_error) result

    val next_value :
      ((module Rec.DeserializerIntf) -> ('value, 'error Error.de_error) result) ->
      ('value option, 'error Error.de_error) result
  end

  module type VisitorIntf = sig
    type visitor
    type value
    type error

    val visit_bool : visitor -> bool -> (value, error Error.de_error) result
    val visit_unit : visitor -> unit -> (value, error Error.de_error) result
    val visit_char : visitor -> char -> (value, error Error.de_error) result
    val visit_int : visitor -> int -> (value, error Error.de_error) result
    val visit_float : visitor -> float -> (value, error Error.de_error) result
    val visit_string : visitor -> string -> (value, error Error.de_error) result

    val visit_seq :
      visitor ->
      (module Rec.DeserializerIntf) ->
      (module Rec.SeqAccessIntf) ->
      (value, error Error.de_error) result

    val visit_variant :
      visitor ->
      (module Rec.DeserializerIntf) ->
      (module Rec.VariantAccessIntf) ->
      (value, error Error.de_error) result

    val visit_map :
      visitor ->
      (module Rec.DeserializerIntf) ->
      (module Rec.MapAccessIntf) ->
      (value, error Error.de_error) result
  end
end

include Rec

module type Intf = DeserializerIntf
module type Map_access_intf = MapAccessIntf
module type Seq_access_intf = SeqAccessIntf
module type Variant_access_intf = VariantAccessIntf
module type Visitor_intf = VisitorIntf
