type rank = { 
  rank_name : string; 
  rank_scores : string list;
}
[@@deriving serializer, deserializer]

type t = { 
  name : string; 
  commisioned : bool;
  updated_at: int64;
  credits: int32 option;
  keywords: string array;
  rank: rank
}
[@@deriving serializer, deserializer]



let () = 
  let test_t = {
    name = "hello";
    commisioned = false;
    updated_at = Int64.(sub max_int 1L);
    credits = None;
    keywords =  [||];
    rank = { rank_name = "asdf"; rank_scores = ["1";"c";"a"]}
  } in
  let json = Serde_json.to_string serialize_t test_t  in
  Format.printf "%s\n%!" json
