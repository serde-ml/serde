open Ppxlib

(** implementation *)

let gen_tuple_field_impl ~ctxt (i, part) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let fn = ser_fun ~ctxt part in
  let f_idx = ("f_" ^ (Int.to_string i)) in
  let pat = Ast_builder.ppat_var ~loc f_idx in
  let var = Ast_builder.pexp_var ~loc f_idx in
  let let_item = Ast_builder
  var, [%stri let [%p pat] = [%e fn] [%e var]]

let gen_serialize_tuple_variant_impl ~ctxt parts =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in

  let (keys, exprs) =
    parts
    |> List.mapi (gen_tuple_field_impl ~ctxt)
    |> List.split in

  [%stri
    [%i exprs ]
    let fields = [%e keys ] in
    Ser.serialize_tuple_variant ~typename:__serde__typename ~variant_idx:1
      ~variant_name:"World" ~variant_size:1 ~fields]

let gen_serialize_variant_ctr_impl ~ctxt ctr =
  match ctr.pcd_args with
  | Pcstr_tuple parts -> gen_serialize_tuple_variant_impl ~ctxt parts
  | Pcstr_record fields -> gen_serialize_record_variant_impl ~ctxt fields

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
