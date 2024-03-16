[@@@warning "-37"]

type rank_externally_tagged =
  | Captain of { name : string; ship : string }
  | Commander of string * int32 * float
  | Lt of bool option
  | Ensign
[@@deriving serialize, deserialize]

type ranks = Ranks of rank_externally_tagged list [@@deriving serialize, deserialize]

let test_externally_tagged () =
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



type rank_adjacently_tagged =
| Captain of { name : string; ship : string }
| Commander of string * int32 * float
| Lt of bool option
| Ensign
[@@deriving serialize, deserialize] [@@serde {tag = "t"; content = "c"}]


  type adjacently_tagged_ranks = Ranks of rank_adjacently_tagged list [@@deriving serialize, deserialize]

  let test_adjacently_tagged () =
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
    let json1 = Serde_json.to_string serialize_adjacently_tagged_ranks test_t |> Result.get_ok in
    (* Format.printf "%s\n%!" json1 *)
    let value = Serde_json.of_string deserialize_adjacently_tagged_ranks json1 |> Result.get_ok in
    let json2 = Serde_json.to_string serialize_adjacently_tagged_ranks value |> Result.get_ok in
    Format.printf "[%s,%s]\n%!" json1 json2
  
let () =
    test_externally_tagged ();
    test_adjacently_tagged ();