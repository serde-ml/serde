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
      'field.
      (module Rec.Visitor_intf with type value = 'value and type tag = 'field) ->
      (module Rec.Visitor_intf with type value = 'field) ->
      fields:string list ->
      ('value, 'error Error.de_error) result;
  }

  type ('value, 'error) map_access = {
    next_key :
      'key.
      deser_key:(unit -> ('key, 'error Error.de_error) result) ->
      ('key option, 'error Error.de_error) result;
    next_value :
      'field.
      deser_value:(unit -> ('field, 'error Error.de_error) result) ->
      ('field option, 'error Error.de_error) result;
  }

  module type Deserializer_base_intf = sig
    type state

    val deserialize_any :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_bool :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_char :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_int :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_float :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_string :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_null :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      ('value option, 'error Error.de_error) result

    val deserialize_unit :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_tuple :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_variant :
      'value 'tag.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value and type tag = 'tag) ->
      (module Rec.Visitor_intf with type value = 'tag) ->
      name:string ->
      variants:string list ->
      ('value, 'error Error.de_error) result

    val deserialize_unit_variant :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_tuple_variant :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_record_variant :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_record :
      'value 'field 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value and type tag = 'field) ->
      (module Rec.Visitor_intf with type value = 'field) ->
      name:string ->
      fields:string list ->
      ('value, 'error Error.de_error) result

    val deserialize_seq :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_map :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_identifier :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result
  end

  module type Deserializer_intf = sig
    include Deserializer_base_intf

    val state : state
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
      'state.
      (module Rec.Visitor_intf with type value = value) ->
      (module Rec.Deserializer_intf with type state = 'state) ->
      (value, 'error) Sequence_access.t ->
      (value, 'error Error.de_error) result

    val visit_variant :
      (tag, value, 'error) variant_access ->
      (value, 'error Error.de_error) result

    val visit_map :
      'state.
      (module Rec.Visitor_intf with type value = value) ->
      (module Rec.Deserializer_intf with type state = 'state) ->
      (value, 'error) map_access ->
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
      'field.
      (module Rec.Visitor_intf with type value = 'value and type tag = 'field) ->
      (module Rec.Visitor_intf with type value = 'field) ->
      fields:string list ->
      ('value, 'error Error.de_error) result;
  }

  module type Deserializer_base_intf = sig
    type state

    val deserialize_any :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_bool :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_char :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_int :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_float :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_string :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_null :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      ('value option, 'error Error.de_error) result

    val deserialize_unit :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_tuple :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_variant :
      'value 'tag.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value and type tag = 'tag) ->
      (module Rec.Visitor_intf with type value = 'tag) ->
      name:string ->
      variants:string list ->
      ('value, 'error Error.de_error) result

    val deserialize_unit_variant :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_tuple_variant :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_record_variant :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_record :
      'value 'field 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value and type tag = 'field) ->
      (module Rec.Visitor_intf with type value = 'field) ->
      name:string ->
      fields:string list ->
      ('value, 'error Error.de_error) result

    val deserialize_seq :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_map :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result

    val deserialize_identifier :
      'value 'error.
      state ->
      (module Rec.Deserializer_intf with type state = state) ->
      (module Rec.Visitor_intf with type value = 'value) ->
      ('value, 'error Error.de_error) result
  end

  module type Deserializer_intf = sig
    include Deserializer_base_intf

    val state : state
  end

  type ('value, 'error) map_access = {
    next_key :
      'key.
      deser_key:(unit -> ('key, 'error Error.de_error) result) ->
      ('key option, 'error Error.de_error) result;
    next_value :
      'field.
      deser_value:(unit -> ('field, 'error Error.de_error) result) ->
      ('field option, 'error Error.de_error) result;
  }

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
      'state.
      (module Rec.Visitor_intf with type value = value) ->
      (module Rec.Deserializer_intf with type state = 'state) ->
      (value, 'error) Sequence_access.t ->
      (value, 'error Error.de_error) result

    val visit_variant :
      (tag, value, 'error) variant_access ->
      (value, 'error Error.de_error) result

    val visit_map :
      'state.
      (module Rec.Visitor_intf with type value = value) ->
      (module Rec.Deserializer_intf with type state = 'state) ->
      (value, 'error) map_access ->
      (value, 'error Error.de_error) result
  end
end

include Rec
