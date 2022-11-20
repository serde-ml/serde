type 'err de_error =
  [> `Duplicate_field of string
  | `Invalid_field_index of int
  | `Invalid_variant_index of int
  | `Message of string
  | `Missing_field of string
  | `Unexpected_exception of exn
  | `Missing_field of string
  | `Unimplemented of string
  | `Unknown_field of string
  | `Unknown_variant of string ]
  as
  'err

let invalid_field_index ~idx = Error (`Invalid_field_index idx)
let invalid_variant_index ~idx = Error (`Invalid_variant_index idx)
let message str = Error (`Message str)
let unexpected_exception exn = Error (`Unexpected_exception exn)
let unimplemented str = Error (`Unimplemented str)
let unknown_field str = Error (`Unknown_field str)
let unknown_variant str = Error (`Unknown_variant str)
let missing_field field = Error (`Missing_field field)
