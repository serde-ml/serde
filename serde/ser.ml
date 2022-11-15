type t

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
