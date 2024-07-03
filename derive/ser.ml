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
  | "array" | "unit" | "option" ->
      true
  | _ -> false

let rec serializer_for_type ~ctxt (core_type : Parsetree.core_type) =
  let loc = loc ~ctxt in
  match core_type.ptyp_desc with
  | Ptyp_constr (name, arg :: []) when is_primitive (Longident.name name.txt) ->
      let type_ser = serializer_for_type ~ctxt arg in
      let name = Ast.pexp_ident ~loc name in
      [%expr s ([%e name] [%e type_ser])]
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

let gen_serialize_externally_tagged_variant_impl ~ctxt ptype_name
    type_attributes cstr_declarations =
  let loc = loc ~ctxt in
  let type_name = Ast.estring ~loc ptype_name.txt in

  let pattern_of_constructor cstr =
    match cstr.pcd_args with
    | Pcstr_tuple [] -> None
    | Pcstr_tuple parts ->
        let gensym = gensym () in
        Some
          (Ast.ppat_tuple ~loc
             (List.map (fun _ -> Ast.pvar ~loc (gensym ~ctxt).txt) parts))
    | Pcstr_record _ -> Some (Ast.pvar ~loc "r")
  in

  let ser_by_constructor type_name idx cstr =
    let idx = Ast.eint ~loc idx in
    let name = Ast.estring ~loc cstr.pcd_name.txt in
    match cstr.pcd_args with
    | Pcstr_tuple [] ->
        [%expr unit_variant ctx [%e type_name] [%e idx] [%e name]]
    | Pcstr_tuple [ arg ] ->
        let ser_fn = serializer_for_type ~ctxt arg in
        let arg_var = Ast.evar ~loc (gensym () ~ctxt).txt in
        let ser = [%expr [%e ser_fn] [%e arg_var]] in
        [%expr newtype_variant ctx [%e type_name] [%e idx] [%e name] [%e ser]]
    | Pcstr_tuple args ->
        let arg_count = Ast.eint ~loc (List.length args) in
        let gensym = gensym () in
        let calls =
          List.mapi
            (fun _idx arg ->
              let ser_fn = serializer_for_type ~ctxt arg in
              let arg_var = Ast.evar ~loc (gensym ~ctxt).txt in
              [%expr element ctx ([%e ser_fn] [%e arg_var])])
            args
        in

        let calls =
          List.fold_left
            (fun last expr ->
              [%expr
                let* () = [%e expr] in
                [%e last]])
            [%expr Ok ()] (List.rev calls)
        in
        [%expr
          tuple_variant ctx [%e type_name] [%e idx] [%e name] [%e arg_count]
            (fun ctx -> [%e calls])]
    | Pcstr_record labels ->
        let field_count = Ast.eint ~loc (List.length labels) in
        let labels = List.rev labels in
        let labels =
          List.map (Attributes.of_field_attributes type_attributes) labels
        in
        let fields =
          List.map
            (fun (field, attr) ->
              let field_name = Ast.estring ~loc Attributes.(attr.name) in
              let field_access =
                let field_name = Longident.parse field.pld_name.txt in
                Ast.pexp_field ~loc (Ast.evar ~loc "r")
                  (Loc.make ~loc field_name)
              in
              let serializer = serializer_for_type ~ctxt field.pld_type in
              [%expr
                field ctx [%e field_name] ([%e serializer] [%e field_access])])
            labels
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
          record_variant ctx [%e type_name] [%e idx] [%e name] [%e field_count]
            (fun ctx -> [%e fields])]
  in

  let cases =
    List.mapi
      (fun idx (cstr : Parsetree.constructor_declaration) ->
        let lhs = Ast.pconstruct cstr (pattern_of_constructor cstr) in
        let rhs = ser_by_constructor type_name idx cstr in
        Ast.case ~lhs ~guard:None ~rhs)
      cstr_declarations
  in

  Ast.pexp_match ~loc [%expr t] cases

let gen_serialize_adjacently_tagged_variant_impl ~tag_field_name
    ~content_field_name ~ctxt ptype_name type_attributes cstr_declarations =
  let loc = loc ~ctxt in
  let type_name = Ast.estring ~loc ptype_name.txt in

  let pattern_of_constructor cstr =
    match cstr.pcd_args with
    | Pcstr_tuple [] -> None
    | Pcstr_tuple parts ->
        let gensym = gensym () in
        Some
          (Ast.ppat_tuple ~loc
             (List.map (fun _ -> Ast.pvar ~loc (gensym ~ctxt).txt) parts))
    | Pcstr_record _ -> Some (Ast.pvar ~loc "r")
  in

  let ser_by_constructor type_name cstr =
    let name = Ast.estring ~loc cstr.pcd_name.txt in
    match cstr.pcd_args with
    | Pcstr_tuple [] ->
        [%expr
          record ctx "" 1 (fun ctx ->
              field ctx [%e Ast.estring ~loc tag_field_name] (string [%e name]))]
    | Pcstr_tuple [ arg ] ->
        let ser_fn = serializer_for_type ~ctxt arg in
        let arg_var = Ast.evar ~loc (gensym () ~ctxt).txt in
        let ser = [%expr [%e ser_fn] [%e arg_var] ctx] in

        [%expr
          record ctx "" 2 (fun ctx ->
              let* () =
                field ctx
                  [%e Ast.estring ~loc tag_field_name]
                  (string [%e name])
              in
              field ctx [%e Ast.estring ~loc content_field_name] (fun ctx ->
                  [%e ser]))]
    | Pcstr_tuple args ->
        let arg_count = Ast.eint ~loc (List.length args) in
        let gensym = gensym () in
        let calls =
          List.mapi
            (fun _idx arg ->
              let ser_fn = serializer_for_type ~ctxt arg in
              let arg_var = Ast.evar ~loc (gensym ~ctxt).txt in
              [%expr element ctx ([%e ser_fn] [%e arg_var])])
            args
        in

        let calls =
          List.fold_left
            (fun last expr ->
              [%expr
                let* () = [%e expr] in
                [%e last]])
            [%expr Ok ()] (List.rev calls)
        in
        [%expr
          record ctx "" 2 (fun ctx ->
              let* () =
                field ctx
                  [%e Ast.estring ~loc tag_field_name]
                  (string [%e name])
              in
              field ctx [%e Ast.estring ~loc content_field_name] (fun ctx ->
                  sequence ctx [%e arg_count] (fun ctx -> [%e calls])))]
    | Pcstr_record labels ->
        let field_count = Ast.eint ~loc (List.length labels) in
        let labels = List.rev labels in
        let labels =
          List.map (Attributes.of_field_attributes type_attributes) labels
        in
        let fields =
          List.map
            (fun (field, attr) ->
              let field_name = Ast.estring ~loc Attributes.(attr.name) in
              let field_access =
                let field_name = Longident.parse field.pld_name.txt in
                Ast.pexp_field ~loc (Ast.evar ~loc "r")
                  (Loc.make ~loc field_name)
              in
              let serializer = serializer_for_type ~ctxt field.pld_type in
              [%expr
                field ctx [%e field_name] ([%e serializer] [%e field_access])])
            labels
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
          record ctx "" 2 (fun ctx ->
              let* () =
                field ctx
                  [%e Ast.estring ~loc tag_field_name]
                  (string [%e name])
              in
              field ctx [%e Ast.estring ~loc content_field_name] (fun ctx ->
                  record ctx [%e type_name] [%e field_count] (fun ctx ->
                      [%e fields])))]
  in

  let cases =
    List.mapi
      (fun _idx (cstr : Parsetree.constructor_declaration) ->
        let lhs = Ast.pconstruct cstr (pattern_of_constructor cstr) in
        let rhs = ser_by_constructor type_name cstr in
        Ast.case ~lhs ~guard:None ~rhs)
      cstr_declarations
  in

  Ast.pexp_match ~loc [%expr t] cases

let gen_serialize_internally_tagged_variant_impl ~tag_field_name:_ ~ctxt:_
    _ptype_name _type_attributes _cstr_declarations =
  failwith "not implemented"

let gen_serialize_variant_impl ~ctxt ptype_name type_attributes
    cstr_declarations =
  match type_attributes.Attributes.variant_tagging_mode with
  | `externally_tagged ->
      gen_serialize_externally_tagged_variant_impl ~ctxt ptype_name
        type_attributes cstr_declarations
  | `internally_tagged tag_field_name ->
      gen_serialize_internally_tagged_variant_impl ~tag_field_name ~ctxt
        ptype_name type_attributes cstr_declarations
  | `adjacently_tagged (tag_field_name, content_field_name) ->
      gen_serialize_adjacently_tagged_variant_impl ~tag_field_name
        ~content_field_name ~ctxt ptype_name type_attributes cstr_declarations
  | _ -> failwith "not implemented"

let gen_serialize_record_impl ~ctxt ptype_name type_attributes
    label_declarations =
  let loc = loc ~ctxt in
  let type_name = Ast.estring ~loc ptype_name.txt in
  let field_count = Ast.eint ~loc (List.length label_declarations) in
  let labels = List.rev label_declarations in
  let labels =
    List.map (Attributes.of_field_attributes type_attributes) labels
  in

  let fields =
    List.map
      (fun (field, attr) ->
        let field_name = Ast.estring ~loc Attributes.(attr.name) in
        let field_access =
          let field_name = Longident.parse field.pld_name.txt in
          Ast.pexp_field ~loc (Ast.evar ~loc "t") (Loc.make ~loc field_name)
        in
        let serializer = serializer_for_type ~ctxt field.pld_type in
        [%expr field ctx [%e field_name] ([%e serializer] [%e field_access])])
      labels
  in

  let fields =
    List.fold_left
      (fun last curr ->
        [%expr
          let* () = [%e curr] in
          [%e last]])
      [%expr Ok ()] fields
  in

  [%expr record ctx [%e type_name] [%e field_count] (fun ctx -> [%e fields])]

let gen_serialize_abstract_impl ~ctxt _type_name core_type =
  let loc = loc ~ctxt in
  let ser = serializer_for_type ~ctxt core_type in
  [%expr [%e ser] t ctx]

let gen_serialize_impl ~ctxt type_decl =
  let loc = loc ~ctxt in

  let typename = type_decl.ptype_name.txt in
  let type_attributes =
    Attributes.of_record_attributes type_decl.ptype_attributes
  in

  let body =
    match type_decl with
    | { ptype_kind = Ptype_record label_declarations; ptype_name; _ } ->
        gen_serialize_record_impl ~ctxt ptype_name type_attributes
          label_declarations
    | { ptype_kind = Ptype_variant cstrs_declaration; ptype_name; _ } ->
        gen_serialize_variant_impl ~ctxt ptype_name type_attributes
          cstrs_declaration
    | {
     ptype_kind = Ptype_abstract;
     ptype_name;
     ptype_manifest = Some core_type;
     _;
    } ->
        gen_serialize_abstract_impl ~ctxt ptype_name core_type
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
      let ( let* ) = Stdlib.Result.bind in
      let _ = ( let* ) in
      Serde.Ser.(fun t ctx -> [%e body])]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = loc ~ctxt in
  List.map (gen_serialize_impl ~ctxt) type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

(** interface *)

let generate_intf ~ctxt:_ (_rec_flag, _type_declarations) = []
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

(** registration *)

let register =
  Deriving.add "serialize" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
