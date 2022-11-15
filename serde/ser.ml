type data = Serde__Data.t
type 'err ser_error = Unimplemented | Custom of 'err

module type Intf = sig
  type output
  type error

  val serialize_bool : output -> bool -> (output, error ser_error) result
  val serialize_unit : output -> unit -> (output, error ser_error) result
  val serialize_char : output -> char -> (output, error ser_error) result
  val serialize_int : output -> int -> (output, error ser_error) result
  val serialize_float : output -> float -> (output, error ser_error) result
  val serialize_string : output -> string -> (output, error ser_error) result

  val serialize_tuple :
    output -> size:int -> elements:data list -> (output, error ser_error) result

  val serialize_unit_variant :
    output ->
    type_name:string ->
    variant_name:string ->
    variant_index:int ->
    (output, error ser_error) result

  val serialize_tuple_variant :
    output ->
    type_name:string ->
    variant_index:int ->
    variant_name:string ->
    variant_size:int ->
    fields:data list ->
    (output, error ser_error) result

  val serialize_record_variant :
    output ->
    type_name:string ->
    variant_index:int ->
    variant_name:string ->
    variant_size:int ->
    fields:data list ->
    (output, error ser_error) result

  val serialize_record :
    output ->
    type_name:string ->
    record_size:int ->
    fields:data list ->
    (output, error ser_error) result
end

module Unimplemented = struct
  let serialize_bool _output _bool = Error Unimplemented
  let serialize_unit _output _unit = Error Unimplemented
  let serialize_char _output _char = Error Unimplemented
  let serialize_int _output _int = Error Unimplemented
  let serialize_float _output _float = Error Unimplemented
  let serialize_string _output _string = Error Unimplemented
  let serialize_tuple _output ~size:_ ~elements:_ = Error Unimplemented

  let serialize_unit_variant _output ~type_name:_ ~variant_name:_
      ~variant_index:_ =
    Error Unimplemented

  let serialize_tuple_variant _output ~type_name:_ ~variant_index:_
      ~variant_name:_ ~variant_size:_ ~fields:_ =
    Error Unimplemented

  let serialize_record_variant _output ~type_name:_ ~variant_index:_
      ~variant_name:_ ~variant_size:_ ~fields:_ =
    Error Unimplemented

  let serialize_record _output ~type_name:_ ~record_size:_ ~fields:_ =
    Error Unimplemented
end

module Make (B : Intf) = struct
  include B
end

let serialize_unit = Ok Data.Unit
let serialize_char value = Ok (Data.Char value)
let serialize_bool value = Ok (Data.Bool value)
let serialize_int value = Ok (Data.Int value)
let serialize_float value = Ok (Data.Float value)
let serialize_string value = Ok (Data.String value)

let serialize_tuple ~size:tup_size ~elements:tup_elements =
  Ok (Data.Tuple { tup_size; tup_elements })

let serialize_unit_variant ~typename:vu_type ~variant_idx:vu_idx
    ~variant_name:vu_name =
  Ok (Data.Variant_unit { vu_type; vu_idx; vu_name })

let serialize_tuple_variant ~typename:vt_type ~variant_idx:vt_idx
    ~variant_name:vt_name ~variant_size:vt_size ~fields:vt_fields =
  Ok (Data.Variant_tuple { vt_type; vt_name; vt_idx; vt_size; vt_fields })

let serialize_record_variant ~typename:vr_type ~variant_idx:vr_idx
    ~variant_name:vr_name ~variant_size:vr_size ~fields:vr_fields =
  Ok (Data.Variant_record { vr_type; vr_name; vr_idx; vr_size; vr_fields })

let serialize_record ~typename:rec_type ~size:rec_size ~fields:rec_fields =
  Ok (Data.Record { rec_type; rec_size; rec_fields })
