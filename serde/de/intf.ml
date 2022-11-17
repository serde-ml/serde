(* NOTE(@ostera): since standalone module signatures can't be made recursive,
   we use this old trick by making a temporary recursive module structure, that
   allows all these signatures to be co-recursive. Ugly, but does the job
*)
module rec Rec : sig
  type ('tag, 'value, 'error) variant_access = {
    tag : unit -> ('tag, 'error Error.de_error) result;
    unit_variant : unit -> (unit, 'error Error.de_error) result;
    tuple_variant :
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result;
    record_variant :
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result;
  }

  module type Deserializer_base_intf = sig
    val deserialize_any :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_bool :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_char :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_int :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_float :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_string :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_unit :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_tuple :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_variant :
      'value 'tag 'val_error 'tag_error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value and type tag = 'tag) ->
      (module Rec.Visitor_intf with type value = 'tag) ->
      name:string ->
      variants:string list ->
      ( 'value,
        [> `Value_error of 'val_error | `Tag_error of 'tag_error ]
        Error.de_error )
      result

    val deserialize_unit_variant :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_tuple_variant :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_record_variant :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_record :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_seq :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_map :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_identifier :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result
  end

  module type Deserializer_intf = sig
    module R : Reader.Instance
    include Deserializer_base_intf
  end

  module type Map_access_intf = sig
    val next_key :
      ((module Rec.Deserializer_intf) -> ('key, 'error Error.de_error) result) ->
      (module Rec.Visitor_intf) ->
      ('key option, 'error Error.de_error) result

    val next_value :
      ((module Rec.Deserializer_intf) -> ('value, 'error Error.de_error) result) ->
      ('value option, 'error Error.de_error) result
  end

  module type Visitor_intf = sig
    type value
    type tag

    val visit_bool : bool -> (value, 'error Error.de_error) result
    val visit_unit : unit -> (unit, 'error Error.de_error) result
    val visit_char : char -> (value, 'error Error.de_error) result
    val visit_int : int -> (value, 'error Error.de_error) result
    val visit_float : float -> (value, 'error Error.de_error) result
    val visit_string : string -> (value, 'error Error.de_error) result

    val visit_seq :
      (module Rec.Visitor_intf with type value = value) ->
      (module Rec.Deserializer_intf) ->
      (value, 'error) Sequence_access.t ->
      (value, 'error Error.de_error) result

    val visit_variant :
      (tag, value, 'error) variant_access ->
      (value, 'error Error.de_error) result

    val visit_map :
      (module Rec.Deserializer_intf) ->
      (module Rec.Map_access_intf) ->
      (value, 'error Error.de_error) result
  end
end = struct
  type ('tag, 'value, 'error) variant_access = {
    tag : unit -> ('tag, 'error Error.de_error) result;
    unit_variant : unit -> (unit, 'error Error.de_error) result;
    tuple_variant :
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result;
    record_variant :
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result;
  }

  module type Deserializer_base_intf = sig
    val deserialize_any :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_bool :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_char :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_int :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_float :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_string :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_unit :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_tuple :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_variant :
      'value 'tag 'val_error 'tag_error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value and type tag = 'tag) ->
      (module Rec.Visitor_intf with type value = 'tag) ->
      name:string ->
      variants:string list ->
      ( 'value,
        [> `Value_error of 'val_error | `Tag_error of 'tag_error ]
        Error.de_error )
      result

    val deserialize_unit_variant :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_tuple_variant :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_record_variant :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_record :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_seq :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_map :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_identifier :
      'value 'error.
      (module Rec.Deserializer_intf) ->
      (module Reader.Instance) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result
  end

  module type Deserializer_intf = sig
    module R : Reader.Instance
    include Deserializer_base_intf
  end

  module type Map_access_intf = sig
    val next_key :
      ((module Rec.Deserializer_intf) -> ('key, 'error Error.de_error) result) ->
      (module Rec.Visitor_intf) ->
      ('key option, 'error Error.de_error) result

    val next_value :
      ((module Rec.Deserializer_intf) -> ('value, 'error Error.de_error) result) ->
      ('value option, 'error Error.de_error) result
  end

  module type Visitor_intf = sig
    type value
    type tag

    val visit_bool : bool -> (value, 'error Error.de_error) result
    val visit_unit : unit -> (unit, 'error Error.de_error) result
    val visit_char : char -> (value, 'error Error.de_error) result
    val visit_int : int -> (value, 'error Error.de_error) result
    val visit_float : float -> (value, 'error Error.de_error) result
    val visit_string : string -> (value, 'error Error.de_error) result

    val visit_seq :
      (module Rec.Visitor_intf with type value = value) ->
      (module Rec.Deserializer_intf) ->
      (value, 'error) Sequence_access.t ->
      (value, 'error Error.de_error) result

    val visit_variant :
      (tag, value, 'error) variant_access ->
      (value, 'error Error.de_error) result

    val visit_map :
      (module Rec.Deserializer_intf) ->
      (module Rec.Map_access_intf) ->
      (value, 'error Error.de_error) result
  end
end

include Rec
