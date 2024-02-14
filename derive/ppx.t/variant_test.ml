[@@@warning "-37"]

type rank = 
  | Captain
  | Commander
  | Lt
  | Ensign 
[@@deriving serialize, deserialize]

let () = 
  let test_t = Ensign in
  let json1 = Serde_json.to_string serialize_rank test_t |> Result.get_ok in
  (* Format.printf "%s\n%!" json1 *)
  let value = Serde_json.of_string deserialize_rank json1 |> Result.get_ok in
  let json2 = Serde_json.to_string serialize_rank value |> Result.get_ok in
  Format.printf "[%s,%s]\n%!" json1 json2
