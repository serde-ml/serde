open Ppxlib
module Ast = Ast_builder.Default

(** helpers *)
let loc ~ctxt = Expansion_context.Deriver.derived_item_loc ctxt

let var ~ctxt name =
  let loc = loc ~ctxt in
  Loc.make ~loc name

(** implementation *)

let rec ser_fun ~ctxt ~v (t : core_type) =
  let loc = loc ~ctxt in
  match t.ptyp_desc with
  (* Serialize a constructor *)
  | Ptyp_constr (name, ty_args) -> (
      match (name.txt |> Longident.name, ty_args) with
      | "bool", _ -> [%expr Ser.serialize_bool [%e v]]
      | "char", _ -> [%expr Ser.serialize_char [%e v]]
      | "float", _ -> [%expr Ser.serialize_float [%e v]]
      | "int", _ -> [%expr Ser.serialize_int [%e v]]
      | "string", _ -> [%expr Ser.serialize_string [%e v]]
      | "unit", _ -> [%expr Ser.serialize_unit [%e v]]
      | "list", [ typ ] ->
          let ser_typ = ser_fun ~ctxt ~v typ in
          [%expr
            let elements = List.map (fun f_0 -> [%e ser_typ]) [%e v] in
            let* elements =
              List.fold_left
                (fun acc el ->
                  match (acc, el) with
                  | Ok ls, Ok el -> Ok (el :: ls)
                  | Ok _, Error err -> Error err
                  | _, _ -> acc)
                (Ok []) elements
            in
            Ser.serialize_seq ~typename:"list" ~elements]
      | _, _ ->
          let ser_fn_name =
            match name.txt |> Longident.flatten_exn |> List.rev with
            | name :: [] -> "serialize_" ^ name
            | name :: path ->
                ("serialize_" ^ name) :: path |> List.rev |> String.concat "."
            | _ -> "unknown"
          in

          let fn =
            ser_fn_name |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc
          in

          Ast.pexp_apply ~loc fn [ (Nolabel, v) ])
  (* Destructure a tuple and deserialize all of its fields *)
  | Ptyp_tuple parts ->
      let pats =
        List.mapi
          (fun i part ->
            let f_idx = "f_" ^ Int.to_string i in
            let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
            let exp =
              f_idx |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc
            in
            (pat, (exp, part)))
          parts
      in

      let destruct, _exprs = List.split pats in
      let vb =
        let destruct = destruct |> Ast.ppat_tuple ~loc in
        [ Ast.value_binding ~loc ~pat:destruct ~expr:v ]
      in

      let keys, exprs =
        parts
        |> List.mapi (fun idx ctyp ->
               let f_idx = "f_" ^ Int.to_string idx in
               let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
               let var =
                 f_idx |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc
               in
               let fn = ser_fun ~ctxt ctyp ~v:var in
               (var, (pat, fn)))
        |> List.split
      in

      let ser_call =
        [%expr
          Ser.serialize_tuple
            ~size:[%e List.length parts |> Ast.eint ~loc]
            ~elements]
      in

      let field_list =
        Ast.pexp_let ~loc Nonrecursive
          [
            Ast.value_binding ~loc
              ~pat:[%pat? elements]
              ~expr:(Ast.elist ~loc keys);
          ]
          ser_call
      in

      Ast.pexp_let ~loc Nonrecursive vb
        (List.fold_left
           (fun body (pat, exp) ->
             let op = var ~ctxt "let*" in
             let let_ = Ast.binding_op ~op ~loc ~pat ~exp in
             Ast.letop ~let_ ~ands:[] ~body |> Ast.pexp_letop ~loc)
           field_list (List.rev exprs))
  (* Unsupported serialization for these *)
  | Ptyp_any | Ptyp_var _
  | Ptyp_object (_, _)
  | Ptyp_class (_, _)
  | Ptyp_alias (_, _)
  | Ptyp_variant (_, _, _)
  | Ptyp_poly (_, _)
  | Ptyp_package _ | Ptyp_extension _
  | Ptyp_arrow (_, _, _) ->
      [%expr ()]

let gen_record_field_impl ~ctxt i ldecl =
  let loc = loc ~ctxt in
  let f_idx = "f_" ^ Int.to_string i in
  let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
  let var = f_idx |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc in
  let fn = ser_fun ~ctxt ldecl.pld_type ~v:var in
  let kv = [%expr [%e ldecl.pld_name.txt |> Ast.estring ~loc], [%e var]] in
  (kv, (pat, fn))

let gen_serialize_record_variant_impl ~ctxt ~typename ~variant_name ~idx fields
    =
  let loc = loc ~ctxt in

  let keys, exprs =
    fields |> List.mapi (gen_record_field_impl ~ctxt) |> List.split
  in

  let ser_call =
    [%expr
      Ser.serialize_record_variant
        ~typename:[%e typename.txt |> Ast.estring ~loc]
        ~variant_idx:[%e idx + 1 |> Ast.eint ~loc]
        ~variant_name:[%e variant_name.txt |> Ast.estring ~loc]
        ~variant_size:[%e List.length keys |> Ast.eint ~loc]
        ~fields]
  in

  let field_list =
    Ast.pexp_let ~loc Nonrecursive
      [ Ast.value_binding ~loc ~pat:[%pat? fields] ~expr:(Ast.elist ~loc keys) ]
      ser_call
  in

  List.fold_left
    (fun body (pat, exp) ->
      let op = var ~ctxt "let*" in
      let let_ = Ast.binding_op ~op ~loc ~pat ~exp in
      Ast.letop ~let_ ~ands:[] ~body |> Ast.pexp_letop ~loc)
    field_list (List.rev exprs)

let gen_tuple_field_impl ~ctxt i ctyp =
  let loc = loc ~ctxt in
  let f_idx = "f_" ^ Int.to_string i in
  let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
  let var = f_idx |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc in
  let fn = ser_fun ~ctxt ctyp ~v:var in
  (var, (pat, fn))

let gen_serialize_tuple_variant_impl ~ctxt ~typename ~variant_name ~idx parts =
  let loc = loc ~ctxt in

  let keys, exprs =
    parts |> List.mapi (gen_tuple_field_impl ~ctxt) |> List.split
  in

  let ser_call =
    [%expr
      Ser.serialize_tuple_variant
        ~typename:[%e typename.txt |> Ast.estring ~loc]
        ~variant_idx:[%e idx + 1 |> Ast.eint ~loc]
        ~variant_name:[%e variant_name.txt |> Ast.estring ~loc]
        ~variant_size:[%e List.length keys |> Ast.eint ~loc]
        ~fields]
  in

  let field_list =
    Ast.pexp_let ~loc Nonrecursive
      [ Ast.value_binding ~loc ~pat:[%pat? fields] ~expr:(Ast.elist ~loc keys) ]
      ser_call
  in

  List.fold_left
    (fun body (pat, exp) ->
      let op = var ~ctxt "let*" in
      let let_ = Ast.binding_op ~op ~loc ~pat ~exp in
      Ast.letop ~let_ ~ands:[] ~body |> Ast.pexp_letop ~loc)
    field_list (List.rev exprs)

let gen_serialize_unit_variant_impl ~ctxt ~typename ~variant_name ~idx =
  let loc = loc ~ctxt in

  [%expr
    Ser.serialize_unit_variant
      ~typename:[%e typename.txt |> Ast.estring ~loc]
      ~variant_idx:[%e idx + 1 |> Ast.eint ~loc]
      ~variant_name:[%e variant_name.txt |> Ast.estring ~loc]]

let gen_serialize_variant_ctr_impl ~ctxt ~typename idx ctr =
  let variant_name = ctr.pcd_name in
  match ctr.pcd_args with
  | Pcstr_tuple [] ->
      gen_serialize_unit_variant_impl ~ctxt ~typename ~variant_name ~idx
  | Pcstr_tuple parts ->
      gen_serialize_tuple_variant_impl ~ctxt ~typename ~variant_name ~idx parts
  | Pcstr_record fields ->
      gen_serialize_record_variant_impl ~ctxt ~typename ~variant_name ~idx
        fields

let gen_serialize_variant_impl ~ctxt typename constructors =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let cases =
    List.mapi
      (fun idx ctr ->
        let args =
          match ctr.pcd_args with
          | Pcstr_tuple [] -> None
          | Pcstr_tuple fields ->
              let fields =
                List.mapi
                  (fun i _ ->
                    let f_idx = "f_" ^ Int.to_string i in
                    let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
                    pat)
                  fields
              in
              Some (Ast.ppat_tuple ~loc fields)
          | Pcstr_record fields ->
              let fields =
                List.mapi
                  (fun i field ->
                    let f_idx = "f_" ^ Int.to_string i in
                    let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
                    (field.pld_name.txt |> Longident.parse |> Loc.make ~loc, pat))
                  fields
              in
              Some (Ast.ppat_record ~loc fields Closed)
        in

        let lhs =
          Ast.ppat_construct ~loc
            (ctr.pcd_name.txt |> Longident.parse |> Loc.make ~loc)
            args
        in
        let rhs = gen_serialize_variant_ctr_impl ~ctxt ~typename idx ctr in
        Ast.case ~lhs ~rhs ~guard:None)
      constructors
  in
  Ast.pexp_match ~loc [%expr t] cases

let gen_serialize_record_impl ~ctxt typename fields =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in

  let extract_fields =
    let fields =
      List.mapi
        (fun i field ->
          let f_idx = "f_" ^ Int.to_string i in
          let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
          (field.pld_name.txt |> Longident.parse |> Loc.make ~loc, pat))
        fields
    in
    Ast.ppat_record ~loc fields Closed
  in

  let keys, exprs =
    fields |> List.mapi (gen_record_field_impl ~ctxt) |> List.split
  in

  let ser_call =
    [%expr
      Ser.serialize_record
        ~typename:[%e typename.txt |> Ast.estring ~loc]
        ~size:[%e List.length keys |> Ast.eint ~loc]
        ~fields]
  in

  let field_list =
    Ast.pexp_let ~loc Nonrecursive
      [ Ast.value_binding ~loc ~pat:[%pat? fields] ~expr:(Ast.elist ~loc keys) ]
      ser_call
  in

  let body =
    List.fold_left
      (fun body (pat, exp) ->
        let op = var ~ctxt "let*" in
        let let_ = Ast.binding_op ~op ~loc ~pat ~exp in
        Ast.letop ~let_ ~ands:[] ~body |> Ast.pexp_letop ~loc)
      field_list (List.rev exprs)
  in

  let t = "t" |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc in
  let vb = [ Ast.value_binding ~loc ~pat:extract_fields ~expr:t ] in
  Ast.pexp_let ~loc Nonrecursive vb body

let gen_serialize_abstract_impl ~ctxt _typename core_type =
  let loc = loc ~ctxt in
  let v = "t" |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc in
  ser_fun ~ctxt ~v core_type

let gen_serialize_impl ~ctxt type_decl =
  let loc = loc ~ctxt in

  let typename = type_decl.ptype_name.txt in

  let body =
    match type_decl with
    | { ptype_kind = Ptype_variant constructors; ptype_name; _ } ->
        gen_serialize_variant_impl ~ctxt ptype_name constructors
    | { ptype_kind = Ptype_record label_declarations; ptype_name; _ } ->
        gen_serialize_record_impl ~ctxt ptype_name label_declarations
    | {
     ptype_kind = Ptype_abstract;
     ptype_name;
     ptype_manifest = Some manifest;
     _;
    } ->
        gen_serialize_abstract_impl ~ctxt ptype_name manifest
    | { ptype_kind; ptype_name; _ } ->
        let err =
          match ptype_kind with
          | Ptype_abstract -> "unsupported abstract type"
          | Ptype_variant _ -> "unsupported variant type"
          | Ptype_record _ -> "unsupported record type"
          | Ptype_open -> "unsupported open type"
        in
        [%expr
          [%e ptype_name.txt |> Ast.estring ~loc] [%e err |> Ast.estring ~loc]]
  in
  let serializer_name =
    "serialize_" ^ typename |> var ~ctxt |> Ast.ppat_var ~loc
  in
  [%stri
    (** Serialize a value of this type into Serde.data *)
    let [%p serializer_name] =
     fun t ->
      let ( let* ) = Result.bind in
      (* NOTE(@ostera): horrible hack to avoid the unused warnings *)
      let* () = Ok () in
      [%e body]]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  List.map (gen_serialize_impl ~ctxt) type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

(** interface *)

let generate_intf ~ctxt:_ (_rec_flag, _type_declarations) = []
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

(** registration *)

let register =
  Deriving.add "serializer" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
