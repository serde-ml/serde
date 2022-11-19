let ( let* ) = Result.bind

let to_string_pretty fn t =
  let* t = fn t in
  let* sexp = Serde.serialize (module Sexpr_ser) t in
  Ok (Sexplib.Sexp.to_string_hum sexp)

let of_string de_fn (str : string) =
  let d = Sexpr_de.of_string str in
  de_fn d
