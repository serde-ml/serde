type data = Serde__Data.t
type 'err ser_error = Unimplemented | Custom of 'err

module type Mapper = sig
  type output
  type error

  val one : data -> (output, error ser_error) result
  val map : data list -> (output list, error ser_error) result

  val map_field :
    (string * data) list -> ((string * output) list, error ser_error) result
end

module type Intf = sig
  type output
  type error

  val initial_output : unit -> (output, error ser_error) result

  val serialize_bool :
    (module Mapper with type output = output and type error = error) ->
    output ->
    bool ->
    (output, error ser_error) result

  val serialize_unit :
    (module Mapper with type output = output and type error = error) ->
    output ->
    unit ->
    (output, error ser_error) result

  val serialize_char :
    (module Mapper with type output = output and type error = error) ->
    output ->
    char ->
    (output, error ser_error) result

  val serialize_int :
    (module Mapper with type output = output and type error = error) ->
    output ->
    int ->
    (output, error ser_error) result

  val serialize_float :
    (module Mapper with type output = output and type error = error) ->
    output ->
    float ->
    (output, error ser_error) result

  val serialize_string :
    (module Mapper with type output = output and type error = error) ->
    output ->
    string ->
    (output, error ser_error) result

  val serialize_tuple :
    (module Mapper with type output = output and type error = error) ->
    output ->
    size:int ->
    elements:data list ->
    (output, error ser_error) result

  val serialize_unit_variant :
    (module Mapper with type output = output and type error = error) ->
    output ->
    type_name:string ->
    variant_name:string ->
    variant_index:int ->
    (output, error ser_error) result

  val serialize_tuple_variant :
    (module Mapper with type output = output and type error = error) ->
    output ->
    type_name:string ->
    variant_index:int ->
    variant_name:string ->
    variant_size:int ->
    fields:data list ->
    (output, error ser_error) result

  val serialize_record_variant :
    (module Mapper with type output = output and type error = error) ->
    output ->
    type_name:string ->
    variant_index:int ->
    variant_name:string ->
    variant_size:int ->
    fields:(string * data) list ->
    (output, error ser_error) result

  val serialize_record :
    (module Mapper with type output = output and type error = error) ->
    output ->
    type_name:string ->
    record_size:int ->
    fields:(string * data) list ->
    (output, error ser_error) result
end

module Unimplemented = struct
  let serialize_bool _ser _output _bool = Error Unimplemented
  let serialize_unit _ser _output _unit = Error Unimplemented
  let serialize_char _ser _output _char = Error Unimplemented
  let serialize_int _ser _output _int = Error Unimplemented
  let serialize_float _ser _output _float = Error Unimplemented
  let serialize_string _ser _output _string = Error Unimplemented
  let serialize_tuple _ser _output ~size:_ ~elements:_ = Error Unimplemented

  let serialize_unit_variant _ser _output ~type_name:_ ~variant_name:_
      ~variant_index:_ =
    Error Unimplemented

  let serialize_tuple_variant _ser _output ~type_name:_ ~variant_index:_
      ~variant_name:_ ~variant_size:_ ~fields:_ =
    Error Unimplemented

  let serialize_record_variant _ser _output ~type_name:_ ~variant_index:_
      ~variant_name:_ ~variant_size:_ ~fields:_ =
    Error Unimplemented

  let serialize_record _ser _output ~type_name:_ ~record_size:_ ~fields:_ =
    Error Unimplemented
end

module Make (B : Intf) = struct
  include B
end

let serialize_unit () = Ok Data.Unit
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

let rec serialize :
    type output error.
    (module Intf with type output = output and type error = error) ->
    data ->
    (output, error ser_error) result =
 fun (module Ser) data ->
  let ( let* ) = Result.bind in

  let module Mapper :
    Mapper with type output = Ser.output and type error = Ser.error = struct
    type output = Ser.output
    type error = Ser.error

    let one data = serialize (module Ser) data

    let rec map xs =
      match xs with
      | [] -> Ok []
      | hd :: tail ->
          let* value = one hd in
          let* tail = map tail in
          Ok (value :: tail)

    let rec map_field xs =
      match xs with
      | [] -> Ok []
      | (name, hd) :: tail ->
          let* value = one hd in
          let* tail = map_field tail in
          Ok ((name, value) :: tail)
  end in
  let* output = Ser.initial_output () in
  match data with
  | Int i -> Ser.serialize_int (module Mapper) output i
  | Bool b -> Ser.serialize_bool (module Mapper) output b
  | Float f -> Ser.serialize_float (module Mapper) output f
  | String s -> Ser.serialize_string (module Mapper) output s
  | Char c -> Ser.serialize_char (module Mapper) output c
  | Tuple { tup_size; tup_elements } ->
      Ser.serialize_tuple
        (module Mapper)
        output ~size:tup_size ~elements:tup_elements
  | Unit -> Ser.serialize_unit (module Mapper) output ()
  | Variant_unit { vu_type; vu_name; vu_idx } ->
      Ser.serialize_unit_variant
        (module Mapper)
        output ~type_name:vu_type ~variant_name:vu_name ~variant_index:vu_idx
  | Variant_tuple { vt_type; vt_name; vt_idx; vt_size; vt_fields } ->
      Ser.serialize_tuple_variant
        (module Mapper)
        output ~type_name:vt_type ~variant_name:vt_name ~variant_index:vt_idx
        ~variant_size:vt_size ~fields:vt_fields
  | Variant_record { vr_type; vr_name; vr_idx; vr_size; vr_fields } ->
      Ser.serialize_record_variant
        (module Mapper)
        output ~type_name:vr_type ~variant_name:vr_name ~variant_index:vr_idx
        ~variant_size:vr_size ~fields:vr_fields
  | Record { rec_type; rec_size; rec_fields } ->
      Ser.serialize_record
        (module Mapper)
        output ~type_name:rec_type ~record_size:rec_size ~fields:rec_fields
