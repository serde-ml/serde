let ( let* ) = Result.bind

let to_string_pretty fn t =
  let* t = fn t in
  let* json = Serde.serialize (module Json_ser) t in
  let yojson = Json.to_yojson json in
  Ok (Yojson.Safe.pretty_to_string yojson)

let of_string de_fn string = de_fn (Json_de.of_string ~string)
