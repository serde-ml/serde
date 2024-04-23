type numbers = int list [@@deriving serialize, deserialize]
type number_list_list = numbers array [@@deriving serialize, deserialize]

let () =
  let test_t = 
    [| [ 1; 2; 3; 4 ]; [4;3;2;1] |] in
  let json1 = Serde_json.to_string serialize_number_list_list test_t |> Result.get_ok in
  let value = Serde_json.of_string deserialize_number_list_list json1 |> Result.get_ok in
  let json2 = Serde_json.to_string serialize_number_list_list value |> Result.get_ok in
  Format.printf "[%s,%s]\n%!" json1 json2
