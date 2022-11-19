open Ppxlib
module Ast = Ast_builder.Default
open De_base

(** implementation *)
let gen_visitor ~ctxt type_name (label_declarations : label_declaration list) =
  let loc = loc ~ctxt in
  let visitor_module_name = "Visitor_for_" ^ type_name.txt in

  let make_vars_and_pats i ldecl =
    let f_idx = "f_" ^ Int.to_string i in
    let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
    let var = f_idx |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc in
    let kv = (longident ~ctxt ldecl.pld_name.txt, var) in
    (kv, (pat, ldecl.pld_type))
  in

  let kvs, parts =
    label_declarations |> List.mapi make_vars_and_pats |> List.split
  in

  let create_value =
    let value = Ast.pexp_record ~loc kvs None in
    [%expr Ok [%e value]]
  in

  let exprs =
    List.map
      (fun (pat, ctyp) ->
        let err_msg =
          Printf.sprintf "%s needs %d argument" type_name.txt
            (List.length parts)
          |> Ast.estring ~loc
        in

        let body =
          [%expr
            let deser_element () =
              [%e de_fun ~ctxt ctyp] (module De) [%e visitor_mod ~ctxt ctyp]
            in
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

  let visitor_module =
    [%str
      include Serde.De.Visitor.Unimplemented

      type value = [%t Ast.ptyp_constr ~loc (longident ~ctxt type_name.txt) []]
      type tag = fields]
    @ [
        [%stri
          let visit_seq :
              (module Serde.De.Visitor.Intf with type value = value) ->
              (module Serde.De.Deserializer) ->
              (value, 'error) Serde.De.Sequence_access.t ->
              (value, 'error Serde.De.Error.de_error) result =
           fun (module Self) (module De) seq_access -> [%e visit_seq]];
      ]
  in

  let visitor_module =
    Ast.module_binding ~loc
      ~name:(var ~ctxt (Some visitor_module_name))
      ~expr:
        (Ast.pmod_apply ~loc
           (Ast.pmod_ident ~loc (longident ~ctxt "Serde.De.Visitor.Make"))
           (Ast.pmod_structure ~loc visitor_module))
    |> Ast.pstr_module ~loc
  in

  let ident =
    let mod_name = Longident.parse visitor_module_name |> var ~ctxt in
    Ast.pexp_pack ~loc (Ast.pmod_ident ~loc mod_name)
  in

  (ident, visitor_module)

let gen_field_visitor ~ctxt type_name constructors =
  let loc = loc ~ctxt in

  let field_visitor_module_name = "Field_visitor_for_" ^ type_name.txt in

  let include_unimplemented = [%stri include Serde.De.Visitor.Unimplemented] in
  let type_value = [%stri type value = fields] in
  let type_tag = [%stri type tag = unit] in

  let visit_int =
    let cases =
      List.mapi
        (fun idx (_field, constructor) ->
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
        (fun (field, constructor) ->
          let idx = Ast.pstring ~loc field.pld_name.txt in
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
      ~name:(var ~ctxt (Some field_visitor_module_name))
      ~expr:
        (Ast.pmod_apply ~loc
           (Ast.pmod_ident ~loc (longident ~ctxt "Serde.De.Visitor.Make"))
           (Ast.pmod_structure ~loc visitor_module))
    |> Ast.pstr_module ~loc
  in

  let ident =
    let mod_name = Longident.parse field_visitor_module_name |> var ~ctxt in
    Ast.pexp_pack ~loc (Ast.pmod_ident ~loc mod_name)
  in

  (ident, visitor_module)

(** Generate deserializer for record types. This generates:

    1 module for the deserializer itself
    1 visitor module for the fields
    1 visitor module for the contents

*)
let gen_deserialize_record_impl ~ctxt type_name
    (label_declarations : label_declaration list) =
  let loc = loc ~ctxt in

  let field_names = List.map (fun l -> l.pld_name.txt) label_declarations in

  let field_constructors =
    List.map (fun l -> (l, "Field_" ^ l.pld_name.txt)) label_declarations
  in
  let fields =
    let kind =
      Ptype_variant
        (List.map
           (fun (_loc, c) ->
             let name = var ~ctxt c in
             Ast.constructor_declaration ~loc ~name ~res:None
               ~args:(Pcstr_tuple []))
           field_constructors)
    in
    [
      Ast.type_declaration ~loc ~name:(var ~ctxt "fields") ~params:[] ~cstrs:[]
        ~kind ~manifest:None ~private_:Public;
    ]
    |> Ast.pstr_type ~loc Nonrecursive
  in

  let constants =
    [
      [%stri let name = [%e type_name.txt |> Ast.estring ~loc]];
      [%stri
        let fields =
          [%e field_names |> List.map (Ast.estring ~loc) |> Ast.elist ~loc]];
      fields;
    ]
  in

  let field_visitor_for_t_name, field_visitor_for_t =
    gen_field_visitor ~ctxt type_name field_constructors
  in

  let visitor_for_t_name, visitor_for_t =
    gen_visitor ~ctxt type_name label_declarations
  in

  let str_items = constants @ [ field_visitor_for_t; visitor_for_t ] in

  let deserialize_body =
    [%expr
      Serde.De.deserialize_record ~name ~fields
        (module De)
        [%e visitor_for_t_name] [%e field_visitor_for_t_name]]
  in

  (str_items, deserialize_body)
