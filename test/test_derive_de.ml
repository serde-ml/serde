open Serde

let ( let* ) = Result.bind

type local = bool [@@deriving serializer]

module Other = struct
  type other = int [@@deriving serializer]
end

type t =
  | Hello
  | Tuple1 of string
  | Tuple2 of string * bool
  | Record3 of { name : string; favorite_number : int; location : string }
[@@deriving serializer, deserializer]

let round_trip str =
  let* t =
    str
    |> Serde_sexpr.of_string deserialize_t
    |> Result.map_error (fun e -> `De e)
  in
  let* sexpr =
    t
    |> Serde_sexpr.to_string_pretty serialize_t
    |> Result.map_error (fun e -> `Ser_sexpr e)
  in
  let* json =
    t
    |> Serde_json.to_string_pretty serialize_t
    |> Result.map_error (fun e -> `Ser_json e)
  in
  Ok (sexpr, json)

let print str =
  match round_trip str with
  | Ok (sexpr, json) ->
      Printf.printf "from: %s\nto (sexpr): %s\nto (json): %s\n\n" str sexpr json;
      String.equal sexpr str
  | Error (`De (`Unimplemented msg)) ->
      Printf.printf "unimplemented: %s\n" msg;
      false
  | Error (`De (`Invalid_variant_index _)) ->
      Printf.printf "invalid_va_idx";
      false
  | Error (`De (`Unknown_variant s)) ->
      Printf.printf "Unknown_variant: %s\n" s;
      false
  | Error (`De (`Duplicate_field _)) ->
      Printf.printf "Duplicate_field";
      false
  | Error (`De (`Missing_field _)) ->
      Printf.printf "Missing_field";
      false
  | Error (`De (`Message msg)) ->
      Printf.printf "Error: %s\n" msg;
      false
  | Error (`Ser _) ->
      Printf.printf "error serializing";
      false
  | _ ->
      Printf.printf "other";
      false

let%test "Deserialize unit variant Hello" = print ":Hello"

(* TODO(@ostera): reenable this after we figure out how to make Sexplib print out string correctly
   let%test "Deserialize tuple variant Tuple1(string)" =
     print "(:Tuple1 (\"asdf\"))"
*)

let%test "Deserialize tuple variant Tuple1(string) with spaces" =
  print "(:Tuple1 (\"a string with spaces\"))"

let%test "Deserialize tuple variant Tuple2(string, bool) with spaces" =
  print "(:Tuple2 (\"a string with spaces\" true))"

let%test "Deserialize tuple variant Tuple2(string, bool) with spaces" =
  print "(:Tuple2 (\"a string with spaces\" false))"

let%test "Deserialize tuple variant Tuple2(string, bool) with spaces" =
  print "(:Record3 (\"Benjamin Sisko\" 9 \"Deep Space 9\"))"
