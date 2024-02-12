type rank = { 
  rank_name : string; 
  rank_score : int32;
}
[@@deriving serializer]

type t = { 
  name : string; 
  commisioned : bool;
  updated_at: int64;
  credits: int32 option;
  keywords: string array;
  rank: rank
}
[@@deriving serializer]
