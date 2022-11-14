open Ppxlib

(** implementation *)

let gen_serialize_variant_ctr_impl ~ctxt ctr =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  

let gen_serialize_variant_impl ~ctxt constructors =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let cases = List.map (gen_serialize_variant_ctr_impl ~ctxt) constructors in
  ([%str match x with c -> .] [@subst let c : list = cases])

let gen_serialize_impl ~ctxt type_decl =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let body =
    match type_decl with
    | { ptype_kind = Ptype_variant constructors; ptype_loc; _ } ->
        gen_serialize_variant_impl ~ctxt constructors
    | { ptype_loc = loc; _ } ->
        let ext =
          Location.error_extensionf ~loc
            "Cannot derive serializer for this type"
        in
        Ast_builder.Default.pstr_extension ~loc
  in
  [%stri
    let serialize t =
      let ( let* ) = Result.bind in
      [%e body]]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  List.map (gen_serialize_impl ~ctxt) type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

(** interface *)

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  List.map (gen_serialize_intf ~ctxt) type_declarations

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

(** registration *)

let deriver =
  Deriving.add "serializer" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
