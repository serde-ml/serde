open Serde
open Serde_json

let keyword fmt = Spices.(default |> fg (color "#00FF00") |> build) fmt
let error fmt = Spices.(default |> fg (color "#FF0000") |> build) fmt

type simple_variant = A
type variant_with_arg = B of int
type variant_with_many_args = C of int * string
type simple_record = { name : string; year : int [@warning "-69"] }
type variant_with_inline_record = D of { is_inline : bool }
type nested = { nested_flag : bool }
type record_nested = { nested : nested }
type with_option = string option
type with_nested_option = { nested_opt : with_option }
type with_list = string list
type with_array = string array

let pp_variant fmt A = Format.fprintf fmt "A"
let pp_variant_with_arg fmt (B i) = Format.fprintf fmt "(B %d)" i

let pp_variant_with_many_arg fmt (C (i, str)) =
  Format.fprintf fmt "(C (%d, %S))" i str

let pp_record fmt { name; year } =
  Format.fprintf fmt "{name=%S;year=%d}" name year

let pp_variant_with_inline_record fmt (D { is_inline }) =
  Format.fprintf fmt "(D {is_inline=%b})" is_inline

let pp_record_nested fmt { nested = { nested_flag } } =
  Format.fprintf fmt "({nested={nested_flag=%b}})" nested_flag

let pp_with_option fmt opt =
  match opt with
  | None -> Format.fprintf fmt "None"
  | Some s -> Format.fprintf fmt "(Some %S)" s

let pp_with_nested_option fmt { nested_opt } =
  Format.fprintf fmt "({nested_opt=%a})" pp_with_option nested_opt

let pp_with_list fmt (t : with_list) =
  Format.fprintf fmt "[";
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
    (fun fmt s -> Format.fprintf fmt "%S" s)
    fmt t;
  Format.fprintf fmt "]"

let pp_with_array fmt (t : with_array) =
  Format.fprintf fmt "[|";
  Format.pp_print_array
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
    (fun fmt s -> Format.fprintf fmt "%S" s)
    fmt t;
  Format.fprintf fmt "|]"

let _serde_json_roundtrip_tests =
  let test str pp ser de value expect_str =
    let actual_str =
      match
        let* json = Serde_json.to_string ser value in
        (* Printf.printf "json: %S\n%!" json; *)
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
    Ser.(serializer @@ fun A ctx -> unit_variant ctx "simple_variant" 0 "A")
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
      let* () = unit_variant ctx in
      Ok A)
    A "A";

  test "variant_without_args and wrong serialization" pp_variant
    Ser.(
      serializer @@ fun A ctx ->
      unit_variant ctx "simple_variant" 0 "Wrong_variant_name")
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
      let* () = unit_variant ctx in
      Ok A)
    A "Exception: invalid_tag";

  test "variant with one arg" pp_variant_with_arg
    Ser.(
      serializer @@ fun (B i) ctx ->
      newtype_variant ctx "simple_variant" 0 "B" (int i))
    De.(
      deserializer @@ fun ctx ->
      let field_visitor =
        Visitor.
          {
            default with
            visit_string =
              (fun _ctx str ->
                match str with "B" -> Ok `B | _ -> Error `invalid_tag);
          }
      in

      variant ctx "simple_variant" [ "B" ] @@ fun ctx ->
      let* `B = identifier ctx field_visitor in
      newtype_variant ctx @@ fun ctx ->
      let* i = int ctx in
      Ok (B i))
    (B 2112) "(B 2112)";

  test "variant with one arg and wrong serialization" pp_variant_with_arg
    Ser.(
      serializer @@ fun (B i) ctx ->
      newtype_variant ctx "simple_variant" 0 "Wrong_variant" (int i))
    De.(
      deserializer @@ fun ctx ->
      let field_visitor =
        Visitor.
          {
            default with
            visit_string =
              (fun _ctx str ->
                match str with "B" -> Ok `B | _ -> Error `invalid_tag);
          }
      in

      variant ctx "simple_variant" [ "B" ] @@ fun ctx ->
      let* `B = identifier ctx field_visitor in
      newtype_variant ctx @@ fun ctx ->
      let* i = int ctx in
      Ok (B i))
    (B 2112) "Exception: invalid_tag";

  test "variant with many args" pp_variant_with_many_arg
    Ser.(
      serializer @@ fun (C (i, str)) ctx ->
      tuple_variant ctx "variant_with_many_args" 0 "C" 2 @@ fun ctx ->
      let* () = element ctx (int i) in
      let* () = element ctx (string str) in
      Ok ())
    De.(
      deserializer @@ fun ctx ->
      let field_visitor =
        Visitor.
          {
            default with
            visit_string =
              (fun _ctx str ->
                match str with "C" -> Ok `C | _ -> Error `invalid_tag);
          }
      in

      variant ctx "variant_with_many_args" [ "C" ] @@ fun ctx ->
      let* `C = identifier ctx field_visitor in
      tuple_variant ctx 2 @@ fun ctx ->
      let* i = element ctx int in
      let i = Option.get i in
      let* str = element ctx string in
      let str = Option.get str in
      Ok (C (i, str)))
    (C (2112, "rush"))
    {|(C (2112, "rush"))|};

  test "record_with_one_arg" pp_record
    Ser.(
      serializer @@ fun r ctx ->
      record ctx "simple_record" 2 @@ fun ctx ->
      let* () = field ctx "name" (string r.name) in
      let* () = field ctx "year" (int r.year) in
      Ok ())
    De.(
      deserializer @@ fun ctx ->
      record ctx "record" 2 @@ fun ctx ->
      let* name = field ctx "name" string in
      let* year = field ctx "year" int in
      Ok { name; year })
    { name = "rush"; year = 2112 }
    {|{name="rush";year=2112}|};

  test "variant with inline record" pp_variant_with_inline_record
    Ser.(
      serializer @@ fun (D { is_inline }) ctx ->
      record_variant ctx "variant_with_many_args" 0 "C" 2 @@ fun ctx ->
      let* () = field ctx "is_inline" (bool is_inline) in
      Ok ())
    De.(
      deserializer @@ fun ctx ->
      let field_visitor =
        Visitor.
          {
            default with
            visit_string =
              (fun _ctx str ->
                match str with "C" -> Ok `C | _ -> Error `invalid_tag);
          }
      in

      variant ctx "variant_with_many_args" [ "C" ] @@ fun ctx ->
      let* `C = identifier ctx field_visitor in
      record_variant ctx 2 @@ fun ctx ->
      let* is_inline = field ctx "is_inline" bool in
      Ok (D { is_inline }))
    (D { is_inline = true })
    {|(D {is_inline=true})|};

  test "record_with_nested_records" pp_record_nested
    Ser.(
      let nested_serializer =
        serializer @@ fun nr ctx ->
        record ctx "nested" 1 @@ fun ctx ->
        field ctx "nested_flag" (bool nr.nested_flag)
      in
      serializer @@ fun r ctx ->
      record ctx "record_nested" 1 @@ fun ctx ->
      field ctx "nested" (s nested_serializer r.nested))
    De.(
      let nested_deserializer =
        deserializer @@ fun ctx ->
        record ctx "record_nested" 1 @@ fun ctx ->
        let* nested_flag = field ctx "nested_flag" bool in
        Ok { nested_flag }
      in
      deserializer @@ fun ctx ->
      record ctx "nested" 1 @@ fun _ctx ->
      let* nested = field ctx "nested" (d nested_deserializer) in
      Ok { nested })
    { nested = { nested_flag = false } }
    {|({nested={nested_flag=false}})|};

  let option_serializer =
    Ser.(serializer @@ fun opt ctx -> option string opt ctx)
  in
  let option_deserializer =
    De.(
      deserializer @@ fun ctx ->
      option ctx @@ fun ctx ->
      let* str = string ctx in
      Ok (Some str))
  in
  test "option/some" pp_with_option option_serializer option_deserializer
    (Some "rush" : with_option)
    {|(Some "rush")|};

  test "option/none" pp_with_option option_serializer option_deserializer None
    {|None|};

  let list_serializer =
    Ser.(
      serializer @@ fun ls ctx ->
      sequence ctx (List.length ls) @@ fun ctx ->
      List.fold_left
        (fun acc el ->
          match acc with Ok () -> element ctx (string el) | _ -> acc)
        (Ok ()) ls)
  in

  let list_deserializer =
    De.(
      deserializer @@ fun ctx ->
      sequence ctx @@ fun ctx ->
      let rec read_elements acc =
        let* v = element ctx string in
        match v with
        | Some s -> read_elements (s :: acc)
        | None -> Ok (List.rev acc)
      in
      read_elements [])
  in
  test "list/empty" pp_with_list list_serializer list_deserializer
    ([] : with_list)
    {|[]|};

  test "list/singleton" pp_with_list list_serializer list_deserializer
    ([ "rush" ] : with_list)
    {|["rush"]|};

  test "list/many" pp_with_list list_serializer list_deserializer
    ([ "rush"; "tom sawyer"; "xanadu"; "2112" ] : with_list)
    {|["rush"; "tom sawyer"; "xanadu"; "2112"]|};

  let array_serializer =
    Ser.(
      serializer @@ fun ls ctx ->
      sequence ctx (Array.length ls) @@ fun ctx ->
      Array.fold_left
        (fun acc el ->
          match acc with Ok () -> element ctx (string el) | _ -> acc)
        (Ok ()) ls)
  in

  let array_deserializer =
    De.(
      deserializer @@ fun ctx ->
      sequence ctx @@ fun ctx ->
      let rec read_elements acc =
        let* v = element ctx string in
        match v with
        | Some s -> read_elements (s :: acc)
        | None -> Ok (Array.of_list (List.rev acc))
      in
      read_elements [])
  in
  test "array/empty" pp_with_array array_serializer array_deserializer
    ([||] : with_array)
    {|[||]|};

  test "array/singleton" pp_with_array array_serializer array_deserializer
    ([| "rush" |] : with_array)
    {|[|"rush"|]|};

  test "array/many" pp_with_array array_serializer array_deserializer
    ([| "rush"; "tom sawyer"; "xanadu"; "2112" |] : with_array)
    {|[|"rush"; "tom sawyer"; "xanadu"; "2112"|]|};

  let nested_opt_ser =
    Ser.(
      serializer @@ fun r ctx ->
      record ctx "with_nested_option" 1 @@ fun ctx ->
      field ctx "nested_opt" (s option_serializer r.nested_opt))
  in
  let nested_opt_de =
    De.(
      deserializer @@ fun ctx ->
      record ctx "with_nested_option" 1 @@ fun ctx ->
      let* nested_opt = field ctx "nested_opt" option_deserializer in
      Ok { nested_opt })
  in
  test "record with nested option/none" pp_with_nested_option nested_opt_ser
    nested_opt_de { nested_opt = None } {|({nested_opt=None})|};
  test "record with nested option/some" pp_with_nested_option nested_opt_ser
    nested_opt_de
    { nested_opt = Some "rush" }
    {|({nested_opt=(Some "rush")})|};
  ()
