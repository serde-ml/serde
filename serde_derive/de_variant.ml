open Ppxlib
module Ast = Ast_builder.Default
open De_base

(** implementation *)
let gen_deserialize_record_variant_impl ~ctxt ~variant_name fields =
  let loc = loc ~ctxt in

  let make_vars_and_pats i (ldecl, field_variant) =
    let record_field_name = ldecl.pld_name.txt in
    let f_idx = "f_" ^ Int.to_string i in
    let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
    let var = f_idx |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc in
    let kv = (longident ~ctxt record_field_name, var) in
    (kv, (record_field_name, pat, ldecl.pld_type, var, field_variant))
  in

  let field_constructors =
    List.map (fun l -> (l, "Field_" ^ l.pld_name.txt)) fields
  in
  let kvs, parts =
    field_constructors |> List.mapi make_vars_and_pats |> List.split
  in

  let fields =
    let kind =
      Ptype_variant
        (List.map
           (fun (_name, _pat, _ctyp, _var, c) ->
             let name = var ~ctxt c in
             Ast.constructor_declaration ~loc ~name ~res:None
               ~args:(Pcstr_tuple []))
           parts)
    in
    [
      Ast.type_declaration ~loc
        ~name:(var ~ctxt ("_fields_" ^ variant_name))
        ~params:[] ~cstrs:[] ~kind ~manifest:None ~private_:Public;
    ]
    |> Ast.pstr_type ~loc Nonrecursive
  in

  let constants =
    [
      [%stri let name = [%e variant_name |> Ast.estring ~loc]];
      [%stri
        let [%p "_fields_" ^ variant_name |> var ~ctxt |> Ast.ppat_var ~loc] =
          [%e
            parts
            |> List.map (fun (name, _pat, _typ, _expr, _variant) ->
                   Ast.estring ~loc name)
            |> Ast.elist ~loc]];
      fields;
    ]
  in

  let type_name = variant_name |> var ~ctxt in

  let field_visitor, field_visitor_module =
    De_record.gen_field_visitor ~ctxt ~type_name
      ~fields_type:
        (let name = "_fields_" ^ variant_name |> longident ~ctxt in
         Ast.ptyp_constr ~loc name [])
      field_constructors
  in

  ( constants @ [ field_visitor_module ],
    [
      [%stri
        type tag =
          [%t
            let name = "_fields_" ^ variant_name |> longident ~ctxt in
            Ast.ptyp_constr ~loc name []]];
      De_record.gen_visit_seq ~ctxt ~type_name ~constructor:variant_name kvs
        parts;
      De_record.gen_visit_map ~ctxt ~type_name ~constructor:variant_name
        ~field_visitor kvs parts;
    ],
    "_fields_" ^ variant_name )

let gen_deserialize_tuple_variant_impl ~ctxt ~variant_name parts =
  let loc = loc ~ctxt in

  let make_vars_and_pats i ctyp =
    let f_idx = "f_" ^ Int.to_string i in
    let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
    let var = f_idx |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc in
    (var, (pat, ctyp))
  in

  let part_idents, parts =
    parts |> List.mapi make_vars_and_pats |> List.split
  in

  let create_value =
    let fields = Ast.pexp_tuple ~loc part_idents in
    let value =
      Ast.pexp_construct ~loc (longident ~ctxt variant_name) (Some fields)
    in
    [%expr Ok [%e value]]
  in

  let exprs =
    List.map
      (fun (pat, ctyp) ->
        let err_msg =
          Printf.sprintf "%s needs %d argument" variant_name (List.length parts)
          |> Ast.estring ~loc
        in

        let deser_element =
          if is_primitive_type ctyp then
            [%expr
              [%e de_fun ~ctxt ctyp]
                (module De)
                [%e visitor_mod ~ctxt ctyp |> Option.get]]
          else [%expr [%e de_fun ~ctxt ctyp] (module De)]
        in

        let body =
          [%expr
            let deser_element () = [%e deser_element] in
            let* r =
              Serde.De.Sequence_access.next_element seq_access ~deser_element
            in
            match r with
            | None -> Serde.De.Error.message (Printf.sprintf [%e err_msg])
            | Some f0 -> Ok f0]
        in

        (pat, body))
      parts
  in

  let visit_seq =
    List.fold_left
      (fun body (pat, exp) ->
        let op = var ~ctxt "let*" in
        let let_ = Ast.binding_op ~op ~loc ~pat ~exp in
        Ast.letop ~let_ ~ands:[] ~body |> Ast.pexp_letop ~loc)
      create_value (List.rev exprs)
  in

  [
    [%stri type tag = unit];
    [%stri
      let visit_seq :
          type state.
          (module Serde.De.Visitor.Intf with type value = value) ->
          (module Serde.De.Deserializer with type state = state) ->
          (value, 'error) Serde.De.Sequence_access.t ->
          (value, 'error Serde.De.Error.de_error) result =
       fun (module Self) (module De) seq_access -> [%e visit_seq]];
  ]

let gen_variant_sub_visitor ~ctxt ~type_name (variant, _constructor) =
  let loc = loc ~ctxt in
  let visitor_module_name = "Variant_visitor_for_" ^ variant.pcd_name.txt in

  let ident =
    let mod_name = visitor_module_name |> Longident.parse |> var ~ctxt in
    Ast.pexp_pack ~loc (Ast.pmod_ident ~loc mod_name)
  in

  let sibling_modules, main_module, tag_type_name =
    let variant_name = variant.pcd_name.txt in
    match variant.pcd_args with
    | Pcstr_tuple [] -> ([], [ [%stri type tag = unit] ], "unit")
    | Pcstr_tuple parts ->
        ( [],
          gen_deserialize_tuple_variant_impl ~ctxt ~variant_name parts,
          "unit" )
    | Pcstr_record fields ->
        gen_deserialize_record_variant_impl ~ctxt ~variant_name fields
  in

  let visitor_module =
    [%str
      include Serde.De.Visitor.Unimplemented

      type value = [%t Ast.ptyp_constr ~loc (longident ~ctxt type_name.txt) []]]
    @ main_module
  in

  let visitor_signature =
    Ast.pmty_with ~loc
      (Ast.pmty_ident ~loc (longident ~ctxt "Serde.De.Visitor.Intf"))
      [
        Pwith_type
          ( longident ~ctxt "value",
            Ast.type_declaration ~loc ~name:(var ~ctxt type_name.txt) ~params:[]
              ~cstrs:[] ~kind:Ptype_abstract
              ~manifest:
                (Some (Ast.ptyp_constr ~loc (longident ~ctxt type_name.txt) []))
              ~private_:Public );
        Pwith_type
          ( longident ~ctxt "tag",
            Ast.type_declaration ~loc ~name:(var ~ctxt tag_type_name) ~params:[]
              ~cstrs:[] ~kind:Ptype_abstract
              ~manifest:
                (Some (Ast.ptyp_constr ~loc (longident ~ctxt tag_type_name) []))
              ~private_:Public );
      ]
  in

  let visitor_module =
    Ast.module_binding ~loc
      ~name:(var ~ctxt (Some visitor_module_name))
      ~expr:
        (Ast.pmod_constraint ~loc
           (Ast.pmod_apply ~loc
              (Ast.pmod_ident ~loc (longident ~ctxt "Serde.De.Visitor.Make"))
              (Ast.pmod_structure ~loc visitor_module))
           visitor_signature)
    |> Ast.pstr_module ~loc
  in

  (ident, sibling_modules @ [ visitor_module ])

let gen_variant_visitor ~ctxt type_name constructors =
  let loc = loc ~ctxt in

  let visitor_module_name = "Visitor_for_" ^ type_name.txt in

  let _variant_modules_ident, variant_modules =
    List.map (gen_variant_sub_visitor ~ctxt ~type_name) constructors
    |> List.split
  in
  let variant_modules = List.flatten variant_modules in

  let visit_variant =
    let cases =
      constructors
      |> List.map (fun (variant, constructor) ->
             let variant_visitor_mod_ident =
               let mod_name =
                 "Variant_visitor_for_" ^ variant.pcd_name.txt
                 |> Longident.parse |> var ~ctxt
               in
               Ast.pexp_pack ~loc (Ast.pmod_ident ~loc mod_name)
             in
             let rhs =
               match variant.pcd_args with
               | Pcstr_tuple [] ->
                   let constructor =
                     Ast.pexp_construct ~loc
                       (longident ~ctxt variant.pcd_name.txt)
                       None
                   in
                   [%expr
                     let* () = Serde.De.Variant_access.unit_variant va in
                     Ok [%e constructor]]
               | Pcstr_tuple _ ->
                   [%expr
                     Serde.De.Variant_access.tuple_variant va
                       [%e variant_visitor_mod_ident]]
               | Pcstr_record _ ->
                   let field_visitor_mod_ident =
                     let mod_name =
                       "Field_visitor_for_" ^ variant.pcd_name.txt
                       |> Longident.parse |> var ~ctxt
                     in
                     Ast.pexp_pack ~loc (Ast.pmod_ident ~loc mod_name)
                   in
                   let fields =
                     "_fields_" ^ variant.pcd_name.txt
                     |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc
                   in
                   [%expr
                     Serde.De.Variant_access.record_variant va
                       ~fields:[%e fields] [%e variant_visitor_mod_ident]
                       [%e field_visitor_mod_ident]]
             in
             Ast.case
               ~lhs:(Ast.ppat_construct ~loc (longident ~ctxt constructor) None)
               ~guard:None ~rhs)
    in
    let match_ = Ast.pexp_match ~loc [%expr tag] cases in

    [%stri
      let visit_variant va =
        let* tag = Serde.De.Variant_access.tag va in
        [%e match_]]
  in

  let visitor_module =
    [%str
      include Serde.De.Visitor.Unimplemented

      type value = [%t Ast.ptyp_constr ~loc (longident ~ctxt type_name.txt) []]
      type tag = variants]
    @ [ visit_variant ]
  in

  let visitor_signature =
    Ast.pmty_with ~loc
      (Ast.pmty_ident ~loc (longident ~ctxt "Serde.De.Visitor.Intf"))
      [
        Pwith_type
          ( longident ~ctxt "value",
            Ast.type_declaration ~loc ~name:(var ~ctxt type_name.txt) ~params:[]
              ~cstrs:[] ~kind:Ptype_abstract
              ~manifest:
                (Some (Ast.ptyp_constr ~loc (longident ~ctxt type_name.txt) []))
              ~private_:Public );
        Pwith_type
          ( longident ~ctxt "tag",
            Ast.type_declaration ~loc ~name:(var ~ctxt "variants") ~params:[]
              ~cstrs:[] ~kind:Ptype_abstract
              ~manifest:
                (Some (Ast.ptyp_constr ~loc (longident ~ctxt "variants") []))
              ~private_:Public );
      ]
  in

  let visitor_module =
    Ast.module_binding ~loc
      ~name:(var ~ctxt (Some visitor_module_name))
      ~expr:
        (Ast.pmod_constraint ~loc
           (Ast.pmod_apply ~loc
              (Ast.pmod_ident ~loc (longident ~ctxt "Serde.De.Visitor.Make"))
              (Ast.pmod_structure ~loc visitor_module))
           visitor_signature)
    |> Ast.pstr_module ~loc
  in

  let ident =
    let mod_name = Longident.parse visitor_module_name |> var ~ctxt in
    Ast.pexp_pack ~loc (Ast.pmod_ident ~loc mod_name)
  in

  (ident, variant_modules @ [ visitor_module ])

let gen_tag_visitor ~ctxt type_name constructors =
  let loc = loc ~ctxt in

  let tag_visitor_module_name = "Tag_visitor_for_" ^ type_name.txt in

  let include_unimplemented = [%stri include Serde.De.Visitor.Unimplemented] in
  let type_value = [%stri type value = variants] in
  let type_tag = [%stri type tag = unit] in

  let visit_int =
    let cases =
      List.mapi
        (fun idx (_variant, constructor) ->
          let idx = Ast.pint ~loc idx in
          let constructor =
            Ast.pexp_construct ~loc (longident ~ctxt constructor) None
          in
          Ast.case
            ~lhs:[%pat? [%p idx]]
            ~guard:None
            ~rhs:[%expr Ok [%e constructor]])
        constructors
    in
    let cases =
      cases
      @ [
          Ast.case
            ~lhs:[%pat? _]
            ~guard:None
            ~rhs:[%expr Serde.De.Error.invalid_variant_index ~idx];
        ]
    in

    let match_ = Ast.pexp_match ~loc [%expr idx] cases in

    [%stri let visit_int idx = [%e match_]]
  in

  let visit_string =
    let cases =
      List.map
        (fun (variant, constructor) ->
          let idx = Ast.pstring ~loc variant.pcd_name.txt in
          let constructor =
            Ast.pexp_construct ~loc (longident ~ctxt constructor) None
          in
          Ast.case
            ~lhs:[%pat? [%p idx]]
            ~guard:None
            ~rhs:[%expr Ok [%e constructor]])
        constructors
    in
    let cases =
      cases
      @ [
          Ast.case
            ~lhs:[%pat? _]
            ~guard:None
            ~rhs:[%expr Serde.De.Error.unknown_variant str];
        ]
    in

    let match_ = Ast.pexp_match ~loc [%expr str] cases in

    [%stri let visit_string str = [%e match_]]
  in

  let visitor_module =
    [ include_unimplemented; type_value; type_tag; visit_int; visit_string ]
  in

  let visitor_module =
    Ast.module_binding ~loc
      ~name:(var ~ctxt (Some tag_visitor_module_name))
      ~expr:
        (Ast.pmod_apply ~loc
           (Ast.pmod_ident ~loc (longident ~ctxt "Serde.De.Visitor.Make"))
           (Ast.pmod_structure ~loc visitor_module))
    |> Ast.pstr_module ~loc
  in

  let ident =
    let mod_name = Longident.parse tag_visitor_module_name |> var ~ctxt in
    Ast.pexp_pack ~loc (Ast.pmod_ident ~loc mod_name)
  in

  (ident, visitor_module)

(** Generate deserializer for variant types. This generates:

    1 module for the deserializer itself
    1 visitor module for the tags
    N visitor modules, one *for each* of the variants themselves

*)
let gen_deserialize_variant_impl ~ctxt type_name constructors =
  let loc = loc ~ctxt in

  let variant_names = List.map (fun c -> c.pcd_name.txt) constructors in

  let variant_constructors =
    List.map (fun c -> (c, "Field_" ^ c.pcd_name.txt)) constructors
  in
  let variants =
    let kind =
      Ptype_variant
        (List.map
           (fun (_loc, c) ->
             let name = var ~ctxt c in
             Ast.constructor_declaration ~loc ~name ~res:None
               ~args:(Pcstr_tuple []))
           variant_constructors)
    in
    [
      Ast.type_declaration ~loc ~name:(var ~ctxt "variants") ~params:[]
        ~cstrs:[] ~kind ~manifest:None ~private_:Public;
    ]
    |> Ast.pstr_type ~loc Nonrecursive
  in

  let constants =
    [
      [%stri let name = [%e type_name.txt |> Ast.estring ~loc]];
      [%stri
        let variants =
          [%e variant_names |> List.map (Ast.estring ~loc) |> Ast.elist ~loc]];
      variants;
    ]
  in

  let visitor_for_t_name, modules =
    gen_variant_visitor ~ctxt type_name variant_constructors
  in

  let tag_visitor_for_t_name, tag_visitor_for_t =
    gen_tag_visitor ~ctxt type_name variant_constructors
  in

  let str_items = constants @ [ tag_visitor_for_t ] @ modules in

  let deserialize_body =
    [%expr
      Serde.De.deserialize_variant ~name ~variants
        (module De)
        [%e visitor_for_t_name] [%e tag_visitor_for_t_name]]
  in

  (str_items, deserialize_body)
