open Serde
open Serde_json

let keyword fmt = Spices.(default |> fg (color "#00FF00") |> build) fmt
let error fmt = Spices.(default |> fg (color "#FF0000") |> build) fmt

type simple_variant = A
(* type variant_with_arg = B of int *)
(* type variant_with_many_args = C of int * string *)
(* type record = { name : string; year : int [@warning "-69"] } *)
(* type variant_with_inline_record = D of { is_inline : bool } *)
(* type nested = { nested_flag : bool } *)
(* type record_nested = { nested : nested } *)

let pp_variant fmt A = Format.fprintf fmt "A"
;;

(* let pp_variant_with_arg fmt (B i) = *)
(*   Format.fprintf fmt "(B %d)" i *)
(* ;; *)
    (* let pp_variant_with_many_arg fmt (C (i, str)) = *)
    (*   Format.fprintf fmt "(C (%d, %S))" i str *)

    (* let pp_record fmt { name; year } = *)
    (*   Format.fprintf fmt "{name=%S;year=%d}" name year *)

    (* let _serde_json_serializer_test = *)
    (*   let test str ser value expected = *)
    (*     let actual_str = Serde_json.to_string ser value |> Result.get_ok in *)
    (*     let expect_str = *)
    (*       Serde_json.Json.to_yojson expected |> Yojson.Safe.pretty_to_string *)
    (*     in *)

    (*     if String.equal actual_str expect_str then *)
    (*       Format.printf "serde_json.ser test %S %s\r\n%!" str (keyword "OK") *)
    (*     else ( *)
    (*       Format.printf "%s\n\nExpected:\n\n%s\n\nbut found:\n\n%s\n\n" *)
    (*         (error "JSON does not match") *)
    (*         expect_str actual_str; *)
    (*       assert false) *)
    (*   in *)

    (*   test "variant_without_args" *)
    (*     Ser.(serializer @@ fun ctx A -> variant ctx "variant" 0 "A" 1) *)
    (*     A *)
    (*     Json.(Object [ ("A", Array []) ]); *)

    (* test "variant_with_one_arg" *)
    (*   Ser.(fun (B i) -> variant "variant" (constructor "B" [ int i ])) *)
    (*   (B 2112) *)
    (*   Json.(Object [ ("B", Array [ Int 2112 ]) ]); *)

    (* test "variant_with_many_args" *)
    (*   Ser.( *)
    (*     fun (C (i, s)) -> variant "variant" (constructor "C" [ int i; string s ])) *)
    (*   (C (2112, "rush")) *)
    (*   Json.(Object [ ("C", Array [ Int 2112; String "rush" ]) ]); *)

    (* test "record_with_one_arg" *)
    (*   Ser.(fun r -> record "record" [ field "name" (string r.name) ]) *)
    (*   { name = "rush"; year = 0 } *)
    (*   Json.(Object [ ("name", String "rush") ]); *)

    (* test "record_with_many_args" *)
    (*   Ser.( *)
    (*     fun r -> *)
    (*       record "record" *)
    (*         [ field "name" (string r.name); field "level" (int r.year) ]) *)
    (*   { name = "rush"; year = 1972 } *)
    (*   Json.(Object [ ("name", String "rush"); ("level", Int 1972) ]); *)

    (* test "variant_with_anonymous_record" *)
    (*   Ser.( *)
    (*     fun (D r) -> *)
    (*       variant_record "var" *)
    (*         (constructor "D" [ field "is_inline" (bool r.is_inline) ])) *)
    (*   (D { is_inline = true }) *)
    (*   Json.(Object [ ("D", Object [ ("is_inline", Bool true) ]) ]); *)

    (* let serialize_nested r = *)
    (*   Ser.(record "nested" [ field "is_nested" (bool r.nested_flag) ]) *)
    (* in *)

    (* test "record_with_nested_record_field" *)
    (*   Ser.( *)
    (*     fun r -> record "record" [ field "nested" (serialize_nested r.nested) ]) *)
    (*   { nested = { nested_flag = true } } *)
    (*   Json.(Object [ ("nested", Object [ ("is_nested", Bool true) ]) ]); *)

    (* test "type_alias" Ser.(alias "age" int) (Alias ("age", Int)); *)
    (*   () *)

    (* let _serde_json_deserializer_test = *)
    (*   let test str pp ser json expect = *)
    (*     let actual_str = *)
    (*       match Serde_json.of_string ser json with *)
    (*       | Ok actual -> Format.asprintf "%a" pp actual *)
    (*       | Error err -> Format.asprintf "Exception: %a" Serde.pp_err err *)
    (*     in *)
    (*     let expect_str = Format.asprintf "%a" pp expect in *)

    (*     if String.equal actual_str expect_str then *)
    (*       Format.printf "serde_json.de test %S %s\r\n%!" str (keyword "OK") *)
    (*     else ( *)
    (*       Format.printf "%s\n\nExpected:\n\n%s\n\nbut found:\n\n%s\n\n" *)
    (*         (error "JSON does not match") *)
    (*         expect_str actual_str; *)
    (*       assert false) *)
    (*   in *)

    (*   test "variant_without_args" pp_variant *)
    (*     De.( *)
    (*       variant "variant" ~tag_of_string:(function *)
    (*         | "A" -> Ok `A *)
    (*         | _ -> Error `invalid_tag) *)
    (*       @@ fun self ctx -> *)
    (*       let* `A = constructor ctx self in *)
    (*       Ok A) *)
    (*     {|"A"|} A; *)

    (*   test "variant_with_one_arg" pp_variant_with_arg *)
    (*     De.( *)
    (*       variant "variant" ~tag_of_string:(function *)
    (*         | "B" -> Ok `B *)
    (*         | _ -> Error `invalid_tag) *)
    (*       @@ fun self ctx -> *)
    (*       let* `B = constructor ctx self in *)
    (*       let* int = int ctx in *)
    (*       Ok (B int)) *)
    (*     {| { "B": [1] } |} (B 1); *)

    (* test "variant_with_many_args" pp_variant_with_many_arg *)
    (*   De.( *)
    (*     variant "variant" *)
    (*       [ *)
    (*         constructor "C" (fun str i -> Ok (C (i, str))) *)
    (*         |> arg string |> arg int; *)
    (*       ]) *)
    (*   {| { "C": [2112, "rush"] } |} *)
    (*   (C (2112, "rush")); *)
    ()

let _serde_json_roundtrip_tests =
  let test str pp ser de value expect_str =
    let actual_str =
      match
        let* json = Serde_json.to_string ser value in
        Printf.printf "json: %S\n%!" json;
        Serde_json.of_string de json
      with
      | Ok actual -> Format.asprintf "%a" pp actual
      | Error err -> Format.asprintf "Exception: %a" Serde.pp_err err
    in

    if String.equal actual_str expect_str then
      Format.printf "serde_json.ser/de test %S %s\r\n%!" str (keyword "OK")
    else (
      Format.printf "%s\n\nExpected:\n\n%s\n\nbut found:\n\n%s\n\n"
        (error "JSON does not match")
        expect_str actual_str;
      assert false)
  in

  test "variant_without_args" pp_variant
    Ser.(serializer @@ fun ctx A -> variant ctx "simple_variant" 0 "A" 0)
    De.(
      deserializer @@ fun ctx ->
      let field_visitor =
        Visitor.
          {
            default with
            visit_string =
              (fun _ctx str ->
                match str with "A" -> Ok `A | _ -> Error `invalid_tag);
          }
      in

      variant ctx "simple_variant" [ "A" ] @@ fun ctx ->
      let* `A = identifier ctx field_visitor in
      Ok A)
    A
    "A"
    ;

  test "variant_without_args and wrong serialization" pp_variant
    Ser.(serializer @@ fun ctx A -> variant ctx "simple_variant" 0 "Wrong_variant_name" 0)
    De.(
      deserializer @@ fun ctx ->
      let field_visitor =
        Visitor.
          {
            default with
            visit_string =
              (fun _ctx str ->
                match str with "A" -> Ok `A | _ -> Error `invalid_tag);
          }
      in

      variant ctx "simple_variant" [ "A" ] @@ fun ctx ->
      let* `A = identifier ctx field_visitor in
      Ok A)
    A
    "Exception: invalid_tag"
    ;

  (* test "variant with one arg" pp_variant *)
  (*   Ser.(serializer @@ fun ctx (B i) ->  *)
  (*     variant ctx "simple_variant" 0 "B" 1 *)
  (*   ) *)
  (*   De.( *)
  (*     deserializer @@ fun ctx -> *)
  (*     let field_visitor = *)
  (*       Visitor. *)
  (*         { *)
  (*           default with *)
  (*           visit_string = *)
  (*             (fun _ctx str -> *)
  (*               match str with "B" -> Ok `B | _ -> Error `invalid_tag); *)
  (*         } *)
  (*     in *)

  (*     variant ctx "simple_variant" [ "A" ] @@ fun ctx -> *)
  (*     let* `B = identifier ctx field_visitor in *)
  (*     let* i = int ctx in *)
  (*     Ok (B i)) *)
  (*   (B 2112) *)
  (*   "A" *)
  (*   ; *)

(*   test "variant_with_one_arg" pp_variant_with_arg *)
(*     Ser.(fun (B i) -> variant "variant" (constructor "B" [ int i ])) *)
(*     De.(variant "variant" [ constructor "B" (fun i -> Ok (B i)) |> arg int ]) *)
(*     (B 2112); *)

(*   test "variant_with_many_args" pp_variant_with_many_arg *)
(*     Ser.( *)
(*       fun (C (i, s)) -> variant "variant" (constructor "C" [ int i; string s ])) *)
(*     De.( *)
(*       variant "variant" *)
(*         [ *)
(*           constructor "C" (fun str i -> Ok (C (i, str))) *)
(*           |> arg string |> arg int; *)
(*         ]) *)
(*     (C (2112, "rush")); *)

(*   test "record_with_one_arg" pp_record *)
(*     Ser.( *)
(*       record "record" (fun ctx r -> *)
(*           let* () = field ctx "name" (string r.name) in *)
(*           field ctx "year" (int r.year))) *)
(*     De.( *)
(*       record "record" (fun ctx -> *)
(*           let* name = field ctx "name" string in *)
(*           let* year = field ctx "year" int in *)
(*           Ok { name; year })) *)
(*     { name = "rush"; year = 2112 }; *)

(*   () *)
