type rank = { rank_scores : string list; rank_name : string }
[@@deriving serialize, deserialize]

type t = {
  name : string;
  commisioned : bool;
  updated_at : int64;
  credits : int32 option;
  keywords : string array;
  rank : rank;
  value : float;
  type_ : string; [@serde { rename = "type" }]
}
[@@deriving serialize, deserialize]

type t_list = { stuff : t list } [@@deriving serialize, deserialize]

let () =
  let test_t =
    {
      stuff =
        [
          {
            name = "hello";
            commisioned = false;
            updated_at = 9223372036854766L;
            credits = None;
            keywords = [||];
            rank = { rank_name = "asdf"; rank_scores = [ "1"; "c"; "a" ] };
            value = 420.69;
            type_ = "something";
          };
          {
            name = "hello";
            commisioned = false;
            updated_at = 0L;
            credits = Some 2112l;
            keywords = [| "hello" |];
            rank = { rank_name = "asdf"; rank_scores = [] };
            value = Float.pi;
            type_ = "something";
          };
        ];
    }
  in
  let json1 = Serde_json.to_string serialize_t_list test_t |> Result.get_ok in
  let value = Serde_json.of_string deserialize_t_list json1 |> Result.get_ok in
  let json2 = Serde_json.to_string serialize_t_list value |> Result.get_ok in
  Format.printf "[%s,%s]\n%!" json1 json2
