open Serde

type local = bool [@@deriving serializer]

module Other = struct
  type other = int [@@deriving serializer]
end

type variant =
  | Hello
  | Nested_tuples of (string * int) * bool * unit * Other.other * local
  | World of string * float
  | Salute of { name : string; role : string }
[@@deriving serializer]

let test =
  [
    serialize_variant Hello;
    serialize_variant (World ("amazing", 0.0));
    serialize_variant (Salute { name = "sisko"; role = "captain" });
  ]
