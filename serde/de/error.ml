type 'err de_error =
  | Unimplemented
  | Invalid_variant_index of { idx : int }
  | Unknown_variant of { str : string }
  | Duplicate_field of string
  | Missing_field of string
  | Custom of 'err
