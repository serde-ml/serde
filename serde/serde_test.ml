open Serde

let keyword fmt = Spices.(default |> fg (color "#00FF00") |> build) fmt
let error fmt = Spices.(default |> fg (color "#FF0000") |> build) fmt

type variant = A
type variant_with_arg = B of int
type variant_with_many_args = C of int * string
type record = { name : string; year : int }
type variant_with_inline_record = D of { is_inline : bool }
type nested = { nested_flag : bool }
type record_nested = { nested : nested }

let _serde_serializer_repr_tests =
  let test str ser value expected =
    let actual = ser value in
    let actual_str = Format.asprintf "%a" Ser.pp actual in
    let expect_str = Format.asprintf "%a" Ser.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "serde.ser test %S %s\r\n%!" str (keyword "OK")
    else (
      Format.printf "%s\n\nExpected:\n\n%a\n\nbut found:\n\n%a\n\n"
        (error "Tokens do not match")
        Ser.pp expected Ser.pp actual;
      assert false)
  in

  test "variant_without_args"
    Ser.(fun A -> variant "variant" ("A", []))
    A
    (Variant_cstr { vcstr_type = "variant"; vcstr_name = "A"; vcstr_args = [] });

  test "variant_with_one_arg"
    Ser.(fun (B i) -> variant "variant" (constructor "B" [ int i ]))
    (B 2112)
    (Variant_cstr
       { vcstr_type = "variant"; vcstr_name = "B"; vcstr_args = [ Int 2112 ] });

  test "variant_with_many_args"
    Ser.(
      fun (C (i, s)) -> variant "variant" (constructor "C" [ int i; string s ]))
    (C (2112, "rush"))
    (Variant_cstr
       {
         vcstr_type = "variant";
         vcstr_name = "C";
         vcstr_args = [ Int 2112; Str "rush" ];
       });

  test "record_with_one_arg"
    Ser.(fun r -> record "record" [ field "name" (string r.name) ])
    { name = "rush"; year = 2112 }
    (Record
       {
         rec_type = "record";
         rec_fields = [ { fld_name = "name"; fld_value = Str "rush" } ];
       });

  test "record_with_many_args"
    Ser.(
      fun r ->
        record "record"
          [ field "name" (string r.name); field "level" (int r.year) ])
    { name = "rush"; year = 1972 }
    (Record
       {
         rec_type = "record";
         rec_fields =
           [
             { fld_name = "name"; fld_value = Str "rush" };
             { fld_name = "level"; fld_value = Int 1972 };
           ];
       });

  test "variant_with_anonymous_record"
    Ser.(
      fun (D r) ->
        variant_record "var"
          (constructor "D" [ field "is_inline" (bool r.is_inline) ]))
    (D { is_inline = true })
    (Variant_record
       {
         vrec_type = "var";
         vrec_name = "D";
         vrec_fields = [ { fld_name = "is_inline"; fld_value = Bool true } ];
       });

  let serialize_nested r =
    Ser.(record "nested" [ field "is_nested" (bool r.nested_flag) ])
  in

  test "record_with_nested_record_field"
    Ser.(
      fun r -> record "record" [ field "nested" (serialize_nested r.nested) ])
    { nested = { nested_flag = true } }
    (Record
       {
         rec_type = "record";
         rec_fields =
           [
             {
               fld_name = "nested";
               fld_value =
                 Record
                   {
                     rec_type = "nested";
                     rec_fields =
                       [ { fld_name = "is_nested"; fld_value = Bool true } ];
                   };
             };
           ];
       });

  (* test "type_alias" Ser.(alias "age" int) (Alias ("age", Int)); *)
  ()

let _serde_deserializer_repr_tests =
  let test str actual expected =
    let actual_str = Format.asprintf "%a" De.pp actual in
    let expect_str = Format.asprintf "%a" De.pp expected in

    if String.equal actual_str expect_str then
      Format.printf "serde.de test %S %s\r\n%!" str (keyword "OK")
    else (
      Format.printf "%s\n\nExpected:\n\n%a\n\nbut found:\n\n%a\n\n"
        (error "Tokens do not match")
        De.pp expected De.pp actual;
      assert false)
  in

  test "variant_without_args"
    De.(variant "variant" [ unit_constructor "A" A ])
    De.(
      Variant
        {
          var_name = "variant";
          var_cstrs = [ Cstr_unit { ucstr_name = "A"; ucstr_val = A } ];
        });

  test "variant_with_one_arg"
    De.(variant "variant" [ constructor "B" (fun i -> Ok (B i)) |> arg int ])
    De.(
      Variant
        {
          var_name = "variant";
          var_cstrs =
            [
              Cstr_args
                {
                  cstr_name = "B";
                  cstr_fn = (fun _config _de _input -> Error `unimplemented);
                };
            ];
        });

  ()