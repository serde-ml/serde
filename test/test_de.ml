open Serde

let ( let* ) = Result.bind

let parse eq fn s t =
  let t' = Serde_sexpr.of_string fn s |> Result.get_ok in
  eq t t'

module Type_alias = struct
  type alias = int [@@deriving serializer, deserializer]

  let parse = parse Int.equal deserialize_alias

  let%test _ = parse "1" 1
end

(*
module Type_abstract = struct
  type abstract [@@deriving serializer, deserializer]
end

module Type_variant = struct
  type variant =
    | Hello
    | Tuple1 of string
    | Tuple2 of string * Type_alias.alias
    | Record3 of { name : string; favorite_number : int; location : string }
  [@@deriving eq, serializer, deserializer]

  let parse = parse equals deserialize_variant

  let%test _ = parse ":Hello" Hello

  let%test _ =
    parse "(:Tuple1 (\"this is a tuple\"))" (Tuple1 "this is a tuple")

  let%test _ =
    parse "(:Tuple2 (\"this is a tuple\"  true))"
      (Tuple2 ("this is a tuple", true))

  let%test _ =
    parse "(:Record3 (\"Benjamin Sisko\" 9 \"Bajor\"))"
      (Record3
         { name = "Benjamin Sisko"; favorite_number = 9; location = "Bajor" })
end

module Type_tuple = struct
  type tuple = int * Type_variant.variant
  [@@deriving eq, serializer, deserializer]
end

module Type_record = struct
  type record = {
    r_name : string;
    r_favorite_number : int;
    r_location : string;
  }
  [@@deriving eq, serializer, deserializer]
end
*)
