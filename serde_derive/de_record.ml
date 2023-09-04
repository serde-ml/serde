open Ppxlib
module Ast = Ast_builder.Default
open De_base

let ctyp_is_option ctyp =
  match ctyp.ptyp_desc with
  | Ptyp_constr (name, _) -> name.txt |> Longident.name = "option"
  | _ -> false

(** implementation *)
let gen_visit_map ~ctxt ~type_name:_ ?constructor ~field_visitor kvs parts =
  let loc = loc ~ctxt in

  let create_value =
    let value = Ast.pexp_record ~loc kvs None in
    let value =
      match constructor with
      | None -> value
      | Some c ->
          Ast.pexp_construct ~loc (longident ~ctxt c)
            (Some (Ast.pexp_record ~loc kvs None))
    in

    [%expr Ok [%e value]]
  in

  let extract_fields =
    List.fold_left
      (fun body (name, pat, ctyp, exp, _field_variant) ->
        let op = var ~ctxt "let*" in
        let exp =
          if ctyp_is_option ctyp then
            [%expr
              match ![%e exp] with
              | Some value -> Ok (Some value)
              | None -> Ok None]
          else
            [%expr
              match ![%e exp] with
              | Some value -> Ok value
              | None ->
                  Serde.De.Error.missing_field [%e name |> Ast.estring ~loc]]
        in
        let let_ = Ast.binding_op ~op ~loc ~pat ~exp in
        Ast.letop ~let_ ~ands:[] ~body |> Ast.pexp_letop ~loc)
      create_value (List.rev parts)
  in

  let fill_individual_field =
    let cases =
      List.map
        (fun (_name, _pat, ctyp, var, field_variant) ->
          let deser_value =
            if is_primitive_type ctyp then
              [%expr
                [%e de_fun ~ctxt ctyp]
                  (module De)
                  [%e visitor_mod ~ctxt ctyp |> Option.get]]
            else [%expr [%e de_fun ~ctxt ctyp] (module De)]
          in

          let assign_field =
            [%expr
              let* value =
                Serde.De.Map_access.next_value map_access
                  ~deser_value:(fun () -> [%e deser_value])
              in
              Ok ([%e var] := value)]
          in
          let constructor =
            Ast.ppat_construct ~loc (longident ~ctxt field_variant) None
          in
          Ast.case ~lhs:[%pat? [%p constructor]] ~guard:None ~rhs:assign_field)
        parts
    in
    let match_ = Ast.pexp_match ~loc [%expr f] cases in
    [%expr
      let* () = [%e match_] in
      fill ()]
  in

  let fill_fields =
    [%expr
      let deser_key () =
        Serde.De.deserialize_identifier (module De) [%e field_visitor]
      in
      let rec fill () =
        let* key = Serde.De.Map_access.next_key map_access ~deser_key in
        match key with None -> Ok () | Some f -> [%e fill_individual_field]
      in
      let* () = fill () in
      [%e extract_fields]]
  in

  let initialize_fields =
    List.fold_left
      (fun body (_name, pat, _type, _var, _field_variant) ->
        let expr = [%expr ref None] in
        let vb = Ast.value_binding ~loc ~pat ~expr in
        Ast.pexp_let ~loc Nonrecursive [ vb ] body)
      fill_fields (List.rev parts)
  in

  [%stri
    let visit_map :
        type de_state.
        value Serde.De.Visitor.t ->
        de_state Serde.De.Deserializer.t ->
        (value, 'error) Serde.De.Map_access.t ->
        (value, 'error Serde.De.Error.de_error) result =
     fun (module Self) (module De) map_access -> [%e initialize_fields]]

let gen_visit_seq ~ctxt ~type_name ?constructor kvs parts =
  let loc = loc ~ctxt in

  let create_value =
    let value = Ast.pexp_record ~loc kvs None in
    let value =
      match constructor with
      | None -> value
      | Some c ->
          Ast.pexp_construct ~loc (longident ~ctxt c)
            (Some (Ast.pexp_record ~loc kvs None))
    in

    [%expr Ok [%e value]]
  in

  let exprs =
    List.map
      (fun (_field_name, pat, ctyp, _expr, _field_variant) ->
        let err_msg =
          Printf.sprintf "%s needs %d argument" type_name.txt
            (List.length parts)
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
          if ctyp_is_option ctyp then
            [%expr
              let deser_element () = [%e deser_element] in
              let* r =
                Serde.De.Sequence_access.next_element seq_access ~deser_element
              in
              match r with
              | None -> Serde.De.Error.message (Printf.sprintf [%e err_msg])
              | Some f0 -> Ok (Some f0)]
          else
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

  [%stri
    let visit_seq :
        type de_state.
        (module Serde.De.Visitor.Intf with type value = value) ->
        (module Serde.De.Deserializer with type state = de_state) ->
        (value, 'error) Serde.De.Sequence_access.t ->
        (value, 'error Serde.De.Error.de_error) result =
     fun (module Self) (module De) seq_access -> [%e visit_seq]]

let gen_visitor ~ctxt ~type_name ~field_visitor
    (label_declarations : (label_declaration * string) list) =
  let loc = loc ~ctxt in
  let visitor_module_name = "Visitor_for_" ^ type_name.txt in

  let make_vars_and_pats i (ldecl, field_variant) =
    let record_field_name = ldecl.pld_name.txt in
    let f_idx = "f_" ^ Int.to_string i in
    let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
    let var = f_idx |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc in
    let kv = (longident ~ctxt record_field_name, var) in
    (kv, (record_field_name, pat, ldecl.pld_type, var, field_variant))
  in

  let labels = label_declarations |> List.mapi make_vars_and_pats in
  let kvs, parts = labels |> List.split in

  let visitor_module =
    [%str
      include Serde.De.Visitor.Unimplemented

      type value = [%t Ast.ptyp_constr ~loc (longident ~ctxt type_name.txt) []]
      type tag = fields]
    @ [
        gen_visit_seq ~ctxt ~type_name kvs parts;
        gen_visit_map ~ctxt ~type_name ~field_visitor kvs parts;
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

let gen_field_visitor ~ctxt ~type_name ?fields_type constructors =
  let loc = loc ~ctxt in

  let field_visitor_module_name = "Field_visitor_for_" ^ type_name.txt in

  let include_unimplemented = [%stri include Serde.De.Visitor.Unimplemented] in
  let type_value =
    match fields_type with
    | Some t -> [%stri type value = [%t t]]
    | None -> [%stri type value = fields]
  in
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
    gen_field_visitor ~ctxt ~type_name field_constructors
  in

  let visitor_for_t_name, visitor_for_t =
    gen_visitor ~ctxt ~type_name ~field_visitor:field_visitor_for_t_name
      field_constructors
  in

  let str_items = constants @ [ field_visitor_for_t; visitor_for_t ] in

  let deserialize_body =
    [%expr
      Serde.De.deserialize_record ~name ~fields
        (module De)
        [%e visitor_for_t_name] [%e field_visitor_for_t_name]]
  in

  (str_items, deserialize_body)
