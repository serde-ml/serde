open Serde

module Serde_bin = struct
  module Serializer = struct
    type output = unit
    type state = S : { fmt : 'w Rio.Writer.t } -> state

    let nest (S { fmt }) = S { fmt }

    let serialize_none _self (S { fmt; _ }) =
      let bytes = Bytes.create 1 in
      Bytes.set_int8 bytes 0 0;
      Rio.write_all fmt ~buf:(Bytes.to_string bytes)

    let serialize_some self (S { fmt; _ }) de =
      let bytes = Bytes.create 1 in
      Bytes.set_int8 bytes 0 1;
      let* () = Rio.write_all fmt ~buf:(Bytes.to_string bytes) in
      Ser.serialize self de

    let serialize_string self (S { fmt; _ }) str =
      let* () = Ser.serialize_int31 (String.length str) self in
      Rio.write_all fmt ~buf:str

    let serialize_bool self (S _) bool =
      let int = if bool then Char.chr 0 else Char.chr 1 in
      Ser.serialize_int8 int self

    let serialize_int8 _self (S { fmt; _ }) int =
      let bytes = Bytes.create 1 in
      Bytes.set bytes 0 int;
      Rio.write_all fmt ~buf:(Bytes.to_string bytes)

    let serialize_int16 _self (S { fmt; _ }) int =
      let bytes = Bytes.create 2 in
      Bytes.set_int16_be bytes 0 int;
      Rio.write_all fmt ~buf:(Bytes.to_string bytes)

    let serialize_int31 _self (S { fmt; _ }) int =
      let bytes = Bytes.create 4 in
      Bytes.set_int32_be bytes 0 (Int32.of_int int);
      Rio.write_all fmt ~buf:(Bytes.to_string bytes)

    let serialize_int32 _self (S { fmt; _ }) int =
      let bytes = Bytes.create 4 in
      Bytes.set_int32_be bytes 0 int;
      Rio.write_all fmt ~buf:(Bytes.to_string bytes)

    let serialize_int64 _self (S { fmt; _ }) int =
      let bytes = Bytes.create 8 in
      Bytes.set_int64_be bytes 0 int;
      Rio.write_all fmt ~buf:(Bytes.to_string bytes)

    let serialize_float _self (S { fmt; _ }) float =
      let bytes = Bytes.create 8 in
      let bof = Int64.bits_of_float float in
      Bytes.set_int64_be bytes 0 bof;
      Rio.write_all fmt ~buf:(Bytes.to_string bytes)

    let serialize_sequence self (S _) ~size elements =
      let* () = Ser.serialize_int31 size self in
      Ser.serialize self elements

    let serialize_element self (S _) element = Ser.serialize self element

    let serialize_unit_variant self (S _) ~var_type:_ ~cstr_idx ~cstr_name:_ =
      Ser.serialize_int31 cstr_idx self

    let serialize_tuple_variant self (S _) ~var_type:_ ~cstr_idx ~cstr_name:_
        ~size args =
      let* () = Ser.serialize_int31 cstr_idx self in
      let* () = Ser.serialize_int31 size self in
      Ser.serialize self args

    let serialize_newtype_variant self (S _) ~var_type:_ ~cstr_idx ~cstr_name:_
        args =
      let* () = Ser.serialize_int31 cstr_idx self in
      Ser.serialize self args

    let serialize_record_variant self (S _) ~var_type:_ ~cstr_idx ~cstr_name:_
        ~size args =
      let* () = Ser.serialize_int31 cstr_idx self in
      let* () = Ser.serialize_int31 size self in
      Ser.serialize self args

    let serialize_record self (S _) ~rec_type:_ ~size:_ fields =
      Ser.serialize self fields

    let serialize_field self (S _) ~name:_ element = Ser.serialize self element
  end

  module Deserializer = struct
    type state =
      | D : {
          reader : 'a Rio.Reader.t;
          mutable size : int;
          mutable off : int;
        }
          -> state

    let nest (D { reader; _ }) = D { reader; size = 0; off = 0 }

    let read capacity (D { reader; _ }) =
      let bytes = Bytes.create capacity in
      let* len = Rio.read reader bytes in
      Ok (Bytes.sub bytes 0 len)

    let read_int capacity state fn =
      let* bytes = read capacity state in
      Ok (fn bytes 0)

    let deserialize_int8 _self state = read_int 1 state Bytes.get
    let deserialize_int16 _self state = read_int 2 state Bytes.get_int16_be

    let deserialize_int31 _self state =
      let* int = read_int 4 state Bytes.get_int32_be in
      Ok (Int32.to_int int)

    let deserialize_int32 _self state = read_int 4 state Bytes.get_int32_be
    let deserialize_int64 _self state = read_int 8 state Bytes.get_int64_be

    let deserialize_float _self state =
      let* float = read_int 8 state Bytes.get_int64_be in
      Ok (Int64.float_of_bits float)

    (* let deserialize_none self _state = *)
    (*   let* int = De.deserialize_int8 self in *)
    (*   if Char.equal int (Char.chr 0) then Ok () *)
    (*   else Error (`Msg "expected 0 to deserialize none") *)

    (* let deserialize_some self _state de = De.deserialize self de *)

    let deserialize_string self state =
      let* len = De.deserialize_int31 self in
      let* bytes = read len state in
      Ok (Bytes.unsafe_to_string bytes)

    let deserialize_bool self _state =
      let* bool = De.deserialize_int8 self in
      if Char.equal bool (Char.chr 0) then Ok true else Ok false

    let deserialize_sequence self (D s) ~size:_ elements =
      let* size = De.deserialize_int31 self in
      s.size <- size;
      De.deserialize self (elements ~size)

    let deserialize_element self (D s) element =
      if s.off < s.size then (
        let* v = De.deserialize self element in
        s.off <- s.off + 1;
        Ok (Some v))
      else Ok None

    let deserialize_unit_variant _self _state = Ok ()
    let deserialize_newtype_variant self _state args = De.deserialize self args

    let deserialize_tuple_variant self (D s) ~size:_ args =
      let* size = De.deserialize_int31 self in
      s.size <- size;
      De.deserialize self (args ~size)

    let deserialize_record_variant self (D s) ~size:_ args =
      let* size = De.deserialize_int31 self in
      s.size <- size;
      De.deserialize self (args ~size)

    let deserialize_record self _state ~name:_ ~size:_ fields =
      De.deserialize self fields

    let deserialize_field self _state ~name:_ element =
      De.deserialize self element

    let deserialize_key _self _state _visitor = Ok None

    let deserialize_identifier self _state visitor =
      let* str = De.deserialize_int31 self in
      Visitor.visit_int self visitor str

    let deserialize_option self _state de =
      let* tag = De.deserialize_int8 self in
      if Char.equal tag (Char.chr 0) then Ok None
      else
        let* v = De.deserialize self de in
        Ok (Some v)

    let deserialize_variant self _state de ~name:_ ~variants:_ =
      De.deserialize self de

    (* TODO: Could probably test this but I don't think it's worth it? *)
    let deserialize_ignored_any _ _ =
      failwith "Should not be called in this test"
  end

  let to_string ser value =
    let buf = Buffer.create 0 in
    let state = Serializer.S { fmt = Rio.Buffer.to_writer buf } in
    let* () = Serde.serialize (module Serializer) state ser value in
    Ok (Buffer.to_bytes buf |> Bytes.unsafe_to_string)

  let of_string de string =
    let module StrRead = struct
      type t = { src : string; mutable pos : int }

      let make src = { src; pos = 0 }

      let read t ?timeout:_ dst =
        let src = t.src in
        let len = Int.min (String.length src - t.pos) (Bytes.length dst) in
        BytesLabels.blit_string ~src ~src_pos:t.pos ~dst ~dst_pos:0 ~len;
        t.pos <- t.pos + len;
        Ok len

      let read_vectored _src _iov = Ok 0
    end in
    let reader =
      Rio.Reader.of_read_src (module StrRead) StrRead.(make string)
    in
    let state = Deserializer.(D { reader; size = 0; off = 0 }) in
    Serde.deserialize (module Deserializer) state de
end

let keyword fmt = Spices.(default |> fg (color "#00FF00") |> build) fmt
let error fmt = Spices.(default |> fg (color "#FF0000") |> build) fmt

type simple_variant = A
type variant_with_arg = B of int
type variant_with_many_args = C of int * string * float
type simple_record = { name : string; year : int [@warning "-69"] }
type variant_with_inline_record = D of { is_inline : bool }
type nested = { nested_flag : bool }
type record_nested = { nested : nested }
type record_with_list = { keys : string list; collection : string }
type with_option = string option
type with_nested_option = { nested_opt : with_option }
type with_list = string list
type with_array = string array

let pp_variant fmt A = Format.fprintf fmt "A"
let pp_variant_with_arg fmt (B i) = Format.fprintf fmt "(B %d)" i

let pp_variant_with_many_arg fmt (C (i, str, float)) =
  Format.fprintf fmt "(C (%d, %S, %F))" i str float

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

let pp_record_with_list fmt { keys; collection } =
  Format.fprintf fmt "{keys=%a;collection=%S}" pp_with_list keys collection

let _serde_bin_roundtrip_tests =
  let test str pp ser de value expect_str =
    let actual_str =
      match
        let* bin = Serde_bin.to_string ser value in
        (* Format.printf "bin: %S for %a\n%!" bin pp value; *)
        Serde_bin.of_string de bin
      with
      | Ok actual -> Format.asprintf "%a" pp actual
      | Error err -> Format.asprintf "Exception: %a" Serde.pp_err err
    in

    if String.equal actual_str expect_str then
      Format.printf "serde_bin.ser/de test %S %s\r\n%!" str (keyword "OK")
    else (
      Format.printf "%s\n\nExpected:\n\n%s\n\nbut found:\n\n%s\n\n"
        (error "bin does not match")
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
            visit_int =
              (fun _ctx int -> if int = 0 then Ok `A else Error `invalid_tag);
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
      unit_variant ctx "simple_variant" 199 "Wrong_variant_name")
    De.(
      deserializer @@ fun ctx ->
      let field_visitor =
        Visitor.
          {
            default with
            visit_int =
              (fun _ctx int -> if int = 0 then Ok `A else Error `invalid_tag);
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
            visit_int =
              (fun _ctx int -> if int = 0 then Ok `B else Error `invalid_tag);
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
      newtype_variant ctx "simple_variant" 990 "Wrong_variant" (int i))
    De.(
      deserializer @@ fun ctx ->
      let field_visitor =
        Visitor.
          {
            default with
            visit_int =
              (fun _ctx int -> if int = 0 then Ok `B else Error `invalid_tag);
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
      serializer @@ fun (C (i, str, flt)) ctx ->
      tuple_variant ctx "variant_with_many_args" 0 "C" 3 @@ fun ctx ->
      let* () = element ctx (int i) in
      let* () = element ctx (string str) in
      let* () = element ctx (float flt) in
      Ok ())
    De.(
      deserializer @@ fun ctx ->
      let field_visitor =
        Visitor.
          {
            default with
            visit_int =
              (fun _ctx int -> if int = 0 then Ok `C else Error `invalid_tag);
          }
      in

      variant ctx "variant_with_many_args" [ "C" ] @@ fun ctx ->
      let* `C = identifier ctx field_visitor in
      tuple_variant ctx 3 @@ fun ~size:_ ctx ->
      let* i = element ctx int in
      let i = Option.get i in
      let* str = element ctx string in
      let str = Option.get str in
      let* flt = element ctx float in
      let flt = Option.get flt in
      Ok (C (i, str, flt)))
    (C (2112, "rush", Float.pi))
    {|(C (2112, "rush", 3.14159265359))|};

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
            visit_int =
              (fun _ctx int -> if int = 0 then Ok `D else Error `invalid_tag);
          }
      in

      variant ctx "variant_with_many_args" [ "C" ] @@ fun ctx ->
      let* `D = identifier ctx field_visitor in
      record_variant ctx 2 @@ fun ~size:_ ctx ->
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
  let option_deserializer = De.(deserializer @@ fun ctx -> option string ctx) in
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
      sequence ctx @@ fun ~size ctx ->
      let rec read_elements size acc =
        if size = 0 then Ok (List.rev acc)
        else
          let* v = element ctx string in
          match v with
          | Some s -> read_elements (size - 1) (s :: acc)
          | None -> Ok (List.rev acc)
      in
      read_elements size [])
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
      sequence ctx @@ fun ~size ctx ->
      let rec read_elements size acc =
        if size = 0 then Ok (List.rev acc)
        else
          let* v = element ctx string in
          match v with
          | Some s -> read_elements (size - 1) (s :: acc)
          | None -> Ok (List.rev acc)
      in
      let* list = read_elements size [] in
      Ok (Array.of_list list))
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

  test "record_with_list" pp_record_with_list
    Ser.(
      serializer @@ fun r ctx ->
      record ctx "record_with_list" 1 @@ fun ctx ->
      let* () = field ctx "keys" (list string r.keys) in
      field ctx "collection" (string r.collection))
    De.(
      deserializer @@ fun ctx ->
      record ctx "record_with_list" 1 @@ fun ctx ->
      let* keys = field ctx "keys" (list string) in
      let* collection = field ctx "collection" string in
      Ok { keys; collection })
    { keys = [ "rush"; "genesis"; "foo fighters" ]; collection = "bands" }
    {|{keys=["rush"; "genesis"; "foo fighters"];collection="bands"}|};

  test "record_with_list/empty" pp_record_with_list
    Ser.(
      serializer @@ fun r ctx ->
      record ctx "record_with_list" 1 @@ fun ctx ->
      let* () = field ctx "keys" (s (list string) r.keys) in
      field ctx "collection" (string r.collection))
    De.(
      deserializer @@ fun ctx ->
      record ctx "record_with_list" 1 @@ fun ctx ->
      let* keys = field ctx "keys" (d (list string)) in
      let* collection = field ctx "collection" string in
      Ok { keys; collection })
    { keys = []; collection = "bands" }
    {|{keys=[];collection="bands"}|};
  ()
