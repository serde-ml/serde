open Ppxlib
module Ast = Ast_builder.Default
open De_base

let gen_deserialize_abstract_tuple_impl ~ctxt type_name parts =
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
    let value = Ast.pexp_tuple ~loc part_idents in
    [%expr Ok [%e value]]
  in

  let exprs =
    List.map
      (fun (pat, ctyp) ->
        let err_msg =
          Printf.sprintf "%s needs %d argument" type_name (List.length parts)
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

  let visitor_module_name = "Abstract_tuple_visitor_for_" ^ type_name in

  let ident =
    let mod_name = visitor_module_name |> Longident.parse |> var ~ctxt in
    Ast.pexp_pack ~loc (Ast.pmod_ident ~loc mod_name)
  in

  let deserialize_body =
    [%expr Serde.De.deserialize_seq (module De) [%e ident]]
  in

  let visitor_module =
    [%str
      include Serde.De.Visitor.Unimplemented

      type value = [%t Ast.ptyp_constr ~loc (longident ~ctxt type_name) []]
      type tag = unit

      let visit_seq :
          type de_state.
          (module Serde.De.Visitor.Intf with type value = value) ->
          (module Serde.De.Deserializer with type state = de_state) ->
          (value, 'error) Serde.De.Sequence_access.t ->
          (value, 'error Serde.De.Error.de_error) result =
       fun (module Self) (module De) seq_access -> [%e visit_seq]]
  in

  let visitor_signature =
    Ast.pmty_with ~loc
      (Ast.pmty_ident ~loc (longident ~ctxt "Serde.De.Visitor.Intf"))
      [
        Pwith_type
          ( longident ~ctxt "value",
            Ast.type_declaration ~loc ~name:(var ~ctxt type_name) ~params:[]
              ~cstrs:[] ~kind:Ptype_abstract
              ~manifest:
                (Some (Ast.ptyp_constr ~loc (longident ~ctxt type_name) []))
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

  ([ visitor_module ], deserialize_body)

(** Generate deserializer for abstract types. This includes:
    
    Aliases:
      type t = int

    Tuples:
      type t = int * int

    Narrowed types:
      type 'ok result = ('ok, string) result

*)
let gen_deserialize_abstract_impl ~ctxt type_name (manifest : core_type) =
  let loc = loc ~ctxt in

  match manifest.ptyp_desc with
  | Ptyp_tuple parts ->
      gen_deserialize_abstract_tuple_impl ~ctxt type_name.txt parts
  | Ptyp_constr (_, []) ->
      let deserialize_body =
        [%expr
          [%e de_fun ~ctxt manifest]
            (module De)
            [%e visitor_mod ~ctxt manifest |> Option.get]]
      in
      ([], deserialize_body)
  | Ptyp_constr (_, _)
  | Ptyp_any | Ptyp_var _
  | Ptyp_arrow (_, _, _)
  | Ptyp_object (_, _)
  | Ptyp_class (_, _)
  | Ptyp_alias (_, _)
  | Ptyp_variant (_, _, _)
  | Ptyp_poly (_, _)
  | Ptyp_package _ | Ptyp_extension _ ->
      ( [],
        Ast.pexp_extension ~loc
        @@ Location.error_extensionf ~loc
             "Can not derive deserializer for abstract type named %s"
             type_name.txt )
