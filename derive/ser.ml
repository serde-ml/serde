open Ppxlib
module Ast = Ast_builder.Default

(** helpers *)
let loc ~ctxt = Expansion_context.Deriver.derived_item_loc ctxt

let var ~ctxt name =
  let loc = loc ~ctxt in
  Loc.make ~loc name

let serializer_fn_name_for_longident name =
  let name =
    match name.txt |> Longident.flatten_exn |> List.rev with
    | name :: [] -> "serialize_" ^ name
    | name :: path ->
        ("serialize_" ^ name) :: path |> List.rev |> String.concat "."
    | _ -> "unknown"
  in
  Longident.parse name

let is_primitive = function
  | "bool" | "char" | "float" | "int" | "int32" | "int64" | "string" | "list"
  | "array" | "unit" ->
      true
  | _ -> false

let rec serializer_for_type ~ctxt (core_type : Parsetree.core_type) =
  let loc = loc ~ctxt in
  match core_type.ptyp_desc with
  | Ptyp_constr (name, arg :: []) when is_primitive (Longident.name name.txt) ->
      let type_ser = serializer_for_type ~ctxt arg in
      let name = Ast.pexp_ident ~loc name in
      [%expr [%e name] [%e type_ser]]
  | Ptyp_constr (name, []) when is_primitive (Longident.name name.txt) ->
      Ast.pexp_ident ~loc name
  | Ptyp_constr (name, _args) ->
      let ser_fn =
        serializer_fn_name_for_longident name
        |> var ~ctxt |> Ast.pexp_ident ~loc
      in
      [%expr s [%e ser_fn]]
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

let gen_serialize_record_impl ~ctxt ptype_name label_declarations =
  let loc = loc ~ctxt in
  let type_name = Ast.estring ~loc ptype_name.txt in
  let field_count = Ast.eint ~loc (List.length label_declarations) in

  let fields =
    List.map
      (fun field ->
        let field_name = Ast.estring ~loc field.pld_name.txt in
        let field_access =
          let field_name = Longident.parse field.pld_name.txt in
          Ast.pexp_field ~loc (Ast.evar ~loc "r") (Loc.make ~loc field_name)
        in
        let serializer = serializer_for_type ~ctxt field.pld_type in
        [%expr field ctx [%e field_name] ([%e serializer] [%e field_access])])
      (List.rev label_declarations)
  in

  let fields =
    List.fold_left
      (fun last curr ->
        [%expr
          let* () = [%e curr] in
          [%e last]])
      [%expr Ok ()] fields
  in

  [%expr
    Serde.Ser.record ctx [%e type_name] [%e field_count] @@ fun ctx ->
    [%e fields]]

let gen_serialize_impl ~ctxt type_decl =
  let loc = loc ~ctxt in

  let typename = type_decl.ptype_name.txt in

  let body =
    match type_decl with
    | { ptype_kind = Ptype_record label_declarations; ptype_name; _ } ->
        gen_serialize_record_impl ~ctxt ptype_name label_declarations
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
    let [%p serializer_name] =
      let ( let* ) = Result.bind in
      Serde.Ser.(fun t ctx -> [%e body])]

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
