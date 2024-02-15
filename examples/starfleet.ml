(* type rank = Captain | Chief_petty_officer *)
(* type t = { name : string; rank : rank } *)

(* let obrien = { name = "Miles O'Brien"; rank = Chief_petty_officer } *)
(* let sisko = { name = "Benjamin Sisko"; rank = Captain } *)

type rank = { rank_name : string; rank_scores : string list }
[@@deriving serialize]

type t = {
  name : string;
  commisioned : bool;
  updated_at : int64;
  credits : int32 option;
  keywords : string array;
  rank : rank;
}
[@@deriving serialize]

let () =
  let test_t =
    {
      name = "hello";
      commisioned = false;
      updated_at = Int64.(sub max_int 1L);
      credits = None;
      keywords = [||];
      rank = { rank_name = "asdf"; rank_scores = [ "1"; "c"; "a" ] };
    }
  in
  let json = Serde_json.to_string serialize_t test_t |> Result.get_ok in
  Format.printf "%s\n%!" json
