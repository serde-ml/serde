module Serializer = Serde__Ser.Make (struct
  include Serde__Ser.Unimplemented

  type output = string

  type error = unit

  let serialize_int output v = Ok (output ^ (Int.to_string v))
end)
