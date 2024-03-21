[@@@warning "-37"]

type rank =
  | Captain of { name : string; ship : string }
  | Commander of string * int32 * float
  | Lt of bool option
  | Ensign
[@@deriving serialize, deserialize] [@@serde {tag = "t"; content = "c"}]

type ranks = Ranks of rank list [@@deriving serialize, deserialize]

let () =
  let test_t =
    Ranks
      [
        Ensign;
        Commander ("riker", 2112l, Float.pi);
        Lt None;
        Lt (Some false);
        Lt (Some true);
        Captain { name = "janeway"; ship = "voyager" };
      ]
  in
  let json1 = Serde_json.to_string serialize_ranks test_t |> Result.get_ok in
  (* Format.printf "%s\n%!" json1 *)
  let value = Serde_json.of_string deserialize_ranks json1 |> Result.get_ok in
  let json2 = Serde_json.to_string serialize_ranks value |> Result.get_ok in
  Format.printf "[%s,%s]\n%!" json1 json2
