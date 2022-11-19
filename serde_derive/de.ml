open Ppxlib
module Ast = Ast_builder.Default
open De_base

let gen_deserialize_impl ~ctxt type_decl =
  let loc = loc ~ctxt in

  let type_name = type_decl.ptype_name.txt in

  let modules, body =
    match type_decl with
    | { ptype_kind = Ptype_variant constructors; ptype_name; _ } ->
        De_variant.gen_deserialize_variant_impl ~ctxt ptype_name constructors
    | { ptype_kind = Ptype_record label_declarations; ptype_name; _ } ->
        De_record.gen_deserialize_record_impl ~ctxt ptype_name
          label_declarations
    | {
     ptype_kind = Ptype_abstract;
     ptype_name;
     ptype_manifest = Some manifest;
     _;
    } ->
        De_abstract.gen_deserialize_abstract_impl ~ctxt ptype_name manifest
    | { ptype_kind; ptype_name; _ } ->
        let err =
          match ptype_kind with
          | Ptype_abstract -> "unsupported abstract type"
          | Ptype_variant _ -> "unsupported variant type"
          | Ptype_record _ -> "unsupported record type"
          | Ptype_open -> "unsupported open type"
        in
        ( [],
          [%expr
            [%e ptype_name.txt |> Ast.estring ~loc] [%e err |> Ast.estring ~loc]]
        )
  in

  let deserializer_module_name = "Serde_deserialize_" ^ type_name in
  let deserializer_fn_name =
    "deserialize_" ^ type_name |> var ~ctxt |> Ast.ppat_var ~loc
  in

  let deserializer_module =
    let fn =
      [%stri
        let [%p deserializer_fn_name] =
         fun (module De : Serde.De.Deserializer) -> [%e body]]
    in
    Ast.pmod_structure ~loc (modules @ [ fn ])
  in

  let deserializer_module =
    Ast.module_binding ~loc
      ~name:(var ~ctxt (Some deserializer_module_name))
      ~expr:deserializer_module
    |> Ast.pstr_module ~loc
  in

  let include_deserializer_module =
    let mod_name = Longident.parse deserializer_module_name |> Loc.make ~loc in
    let include_decl = Ast.include_infos ~loc (Ast.pmod_ident ~loc mod_name) in
    Ast.pstr_include ~loc include_decl
  in

  [ deserializer_module; include_deserializer_module ]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  List.map (gen_deserialize_impl ~ctxt) type_declarations |> List.flatten

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

(** interface *)

let generate_intf ~ctxt:_ (_rec_flag, _type_declarations) = []
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

(** registration *)

let register =
  Deriving.add "deserializer" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
