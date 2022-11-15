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

type r = { my_field : variant; hello : string } [@@deriving serializer]

let _ =
  let ( let* ) = Result.bind in
  [
    serialize_local true;
    Other.serialize_other 2112;
    serialize_variant Hello;
    serialize_variant (World ("amazing", 0.0));
    serialize_variant (Salute { name = "sisko"; role = "captain" });
    serialize_variant (Nested_tuples (("oops", 1), true, (), 2112, true));
    serialize_r
      {
        my_field = Salute { name = "sisko"; role = "captain" };
        hello = "world";
      };
  ]
  |> List.map (fun d ->
         let* value = d in
         let* sexpr = Serde_xml.to_string_pretty value in
         print_newline ();
         print_string sexpr;
         print_newline ();
         Ok ())
  |> ignore;
  Ok ()
