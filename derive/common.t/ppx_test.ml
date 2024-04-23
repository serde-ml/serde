type numbers = int list [@@deriving serializer, deserializer]

let () =
  let test_t = [ 1; 2; 3; 4 ] in
  let json1 = Serde_json.to_string serialize_numbers test_t |> Result.get_ok in
  let value = Serde_json.of_string deserialize_numbers json1 |> Result.get_ok in
  let json2 = Serde_json.to_string serialize_numbers value |> Result.get_ok in
  Format.printf "[%s,%s]\n%!" json1 json2
