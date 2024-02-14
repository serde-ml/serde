open Ppxlib
module Ast = Ast_builder.Default

(** helpers *)
let loc ~ctxt = Expansion_context.Deriver.derived_item_loc ctxt

let var ~ctxt name =
  let loc = loc ~ctxt in
  Loc.make ~loc name

let gensym () =
  let counter = ref 0 in
  fun ~ctxt ->
    counter := !counter + 1;
    var ~ctxt ("v_" ^ Int.to_string !counter)

let deserializer_fn_name_for_longident name =
  let name =
    match name.txt |> Longident.flatten_exn |> List.rev with
    | name :: [] -> "deserialize_" ^ name
    | name :: path ->
        ("deserialize_" ^ name) :: path |> List.rev |> String.concat "."
    | _ -> "unknown"
  in
  Longident.parse name

let is_primitive = function
  | "bool" | "char" | "float" | "int" | "int32" | "int64" | "string" | "list"
  | "array" | "unit" | "option" ->
      true
  | _ -> false

let rec deserializer_for_type ~ctxt (core_type : Parsetree.core_type) =
  let loc = loc ~ctxt in
  match core_type.ptyp_desc with
  | Ptyp_constr (name, arg :: []) when is_primitive (Longident.name name.txt) ->
      let type_ser = deserializer_for_type ~ctxt arg in
      let name = Ast.pexp_ident ~loc name in
      [%expr d ([%e name] [%e type_ser])]
  | Ptyp_constr (name, []) when is_primitive (Longident.name name.txt) ->
      Ast.pexp_ident ~loc name
  | Ptyp_constr (name, _args) ->
      let ser_fn =
        deserializer_fn_name_for_longident name
        |> var ~ctxt |> Ast.pexp_ident ~loc
      in
      [%expr d [%e ser_fn]]
  | Ptyp_any | Ptyp_var _
  | Ptyp_arrow (_, _, _)
  | Ptyp_tuple _
  | Ptyp_object (_, _)
  | Ptyp_class (_, _)
  | Ptyp_alias (_, _)
  | Ptyp_variant (_, _, _)
  | Ptyp_poly (_, _)
  | Ptyp_package _ | Ptyp_extension _ ->
      failwith "unsupported"

(** implementation *)

let gen_serialize_variant_impl ~ctxt ptype_name cstr_declarations =
  let loc = loc ~ctxt in
  let type_name = Ast.estring ~loc ptype_name.txt in
  let constructor_names =
    Ast.elist ~loc
      (List.map
         (fun (cstr : Parsetree.constructor_declaration) ->
           Ast.estring ~loc cstr.pcd_name.txt)
         cstr_declarations)
  in

  let deser_by_constructor _type_name idx cstr =
    let _idx = Ast.eint ~loc idx in
    let name = Longident.parse cstr.pcd_name.txt |> var ~ctxt in
    match cstr.pcd_args with
    | Pcstr_tuple [] ->
        let value = Ast.pexp_construct ~loc name None in
        [%expr
          let* () = unit_variant ctx in
          Ok [%e value]]
    | Pcstr_tuple [ arg ] ->
        let sym = gensym () ~ctxt in
        let arg_pat = Ast.pvar ~loc sym.txt in
        let arg_var = Ast.evar ~loc sym.txt in

        let value =
          let cstr = Ast.pexp_construct ~loc name (Some arg_var) in
          [%expr Ok [%e cstr]]
        in

        let ser_fn = deserializer_for_type ~ctxt arg in
        let body =
          [%expr
            let* [%p arg_pat] = [%e ser_fn] ctx in
            [%e value]]
        in

        [%expr newtype_variant ctx @@ fun ctx -> [%e body]]
    | _ -> [%expr Obj.magic 1]
  in

  let tag_dispatch =
    let cases =
      List.mapi
        (fun idx (cstr : Parsetree.constructor_declaration) ->
          let lhs = Ast.ppat_variant ~loc cstr.pcd_name.txt None in
          let rhs = deser_by_constructor type_name idx cstr in
          Ast.case ~lhs ~guard:None ~rhs)
        cstr_declarations
    in

    Ast.pexp_match ~loc [%expr tag] cases
  in

  let field_visitor =
    let cases =
      List.mapi
        (fun _idx (cstr : Parsetree.constructor_declaration) ->
          let tag_name = cstr.pcd_name.txt in
          let lhs = Ast.ppat_constant ~loc (Ast_helper.Const.string tag_name) in
          let rhs =
            let tag = Ast.pexp_variant ~loc cstr.pcd_name.txt None in
            [%expr Ok [%e tag]]
          in
          Ast.case ~lhs ~guard:None ~rhs)
        cstr_declarations
      @ [
          Ast.case ~lhs:(Ast.ppat_any ~loc) ~guard:None
            ~rhs:[%expr Error `invalid_tag];
        ]
    in

    let tag_match = Ast.pexp_match ~loc [%expr str] cases in

    [%expr Visitor.make ~visit_string:(fun _ctx str -> [%e tag_match]) ()]
  in

  [%expr
    let field_visitor = [%e field_visitor] in
    variant ctx [%e type_name] [%e constructor_names] @@ fun ctx ->
    let* tag = identifier ctx field_visitor in
    [%e tag_dispatch]]

let gen_serialize_record_impl ~ctxt ptype_name label_declarations =
  let loc = loc ~ctxt in
  let type_name = Ast.estring ~loc ptype_name.txt in
  let field_count = Ast.eint ~loc (List.length label_declarations) in

  let record_expr =
    let fields =
      List.map
        (fun field ->
          let value = Ast.evar ~loc field.pld_name.txt in
          let field = Longident.parse field.pld_name.txt |> var ~ctxt in
          (field, value))
        label_declarations
    in
    let record = Ast.pexp_record ~loc fields None in
    [%expr Ok [%e record]]
  in

  let fields =
    List.map
      (fun field ->
        let field_name = Ast.estring ~loc field.pld_name.txt in
        let deserializer = deserializer_for_type ~ctxt field.pld_type in
        let de_expr = [%expr field ctx [%e field_name] [%e deserializer]] in
        let field_name = field.pld_name.txt in
        (field_name, de_expr))
      (List.rev label_declarations)
  in

  let fields =
    List.fold_left
      (fun last (field, expr) ->
        let field = Ast.(pvar ~loc field) in
        [%expr
          let* [%p field] = [%e expr] in
          [%e last]])
      record_expr fields
  in

  [%expr record ctx [%e type_name] [%e field_count] (fun ctx -> [%e fields])]

let gen_serialize_impl ~ctxt type_decl =
  let loc = loc ~ctxt in

  let typename = type_decl.ptype_name.txt in

  let body =
    match type_decl with
    | { ptype_kind = Ptype_record label_declarations; ptype_name; _ } ->
        gen_serialize_record_impl ~ctxt ptype_name label_declarations
    | { ptype_kind = Ptype_variant cstrs_declaration; ptype_name; _ } ->
        gen_serialize_variant_impl ~ctxt ptype_name cstrs_declaration
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
  let deserializer_name =
    "deserialize_" ^ typename |> var ~ctxt |> Ast.ppat_var ~loc
  in
  [%stri
    let [%p deserializer_name] =
      let ( let* ) = Result.bind in
      Serde.De.(fun ctx -> [%e body])]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = loc ~ctxt in
  [ [%stri open! Serde]; [%stri let ( let* ) = Result.bind] ]
  @ List.map (gen_serialize_impl ~ctxt) type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

(** interface *)

let generate_intf ~ctxt:_ (_rec_flag, _type_declarations) = []
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

(** registration *)

let register =
  Deriving.add "deserialize" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
