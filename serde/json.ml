(*
module Seralizer = Ser.Make (struct
  let serialize_int acc v =
    acc := !acc ^ Int.to_string v;
    Ok ()
end)

module Deserializer (V : Visitor) = struct
  include De.Make (struct
    type error = Syntax

    let deserialize_any de v =
      match De.peek_char de with
      | '0' .. '9' -> deserialize_int v
      | _ -> Error Syntax

    let deserialize_int de v =
      let* i = De.parse_int de in
      let* v = V.visit_int v i in
      Ok v
  end)
end
*)
