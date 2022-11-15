open Ppxlib
module Ast = Ast_builder.Default

(** helpers *)
let loc ~ctxt = Expansion_context.Deriver.derived_item_loc ctxt

let var ~ctxt name =
  let loc = loc ~ctxt in
  Loc.make ~loc name

(** implementation *)

let ser_fun ~ctxt (_v : core_type) =
  let loc = loc ~ctxt in
  [%expr ()]

let gen_tuple_field_impl ~ctxt i ctyp =
  let loc = loc ~ctxt in
  let fn = ser_fun ~ctxt ctyp in
  let f_idx = "f_" ^ Int.to_string i in
  let pat = f_idx |> var ~ctxt |> Ast.ppat_var ~loc in
  let var = f_idx |> Longident.parse |> var ~ctxt |> Ast.pexp_ident ~loc in
  (var, (pat, [%expr [%e fn] [%e var]]))

let gen_serialize_tuple_variant_impl ~ctxt ~typename ~variant_name ~idx parts =
  let loc = loc ~ctxt in

  let keys, exprs =
    parts |> List.mapi (gen_tuple_field_impl ~ctxt) |> List.split
  in

  let ser_call =
    [%expr
      Ser.serialize_tuple_variant
        ~typename:[%e typename.txt |> Ast.estring ~loc]
        ~variant_idx:[%e idx + 1 |> Ast.eint ~loc]
        ~variant_name:[%e variant_name.txt |> Ast.estring ~loc]
        ~variant_size:[%e List.length keys |> Ast.eint ~loc]
        ~fields]
  in

  let field_list =
    Ast.pexp_let ~loc Nonrecursive
      [
        Ast.value_binding ~loc
          ~pat:[%pat? fields]
          ~expr:(Ast.esequence ~loc keys);
      ]
      ser_call
  in

  Ast.letop
    ~let_:
      (Ast.binding_op ~loc ~op:(var ~ctxt "let*")
         ~pat:[%pat? ( let* )]
         ~exp:[%expr Result.bind])
    ~ands:
      (List.map
         (fun (pat, exp) ->
           Ast.binding_op ~loc ~pat ~exp ~op:(var ~ctxt "let*"))
         exprs)
    ~body:field_list
  |> Ast.pexp_letop ~loc

let gen_serialize_variant_ctr_impl ~ctxt ~typename idx ctr =
  let variant_name = ctr.pcd_name in
  match ctr.pcd_args with
  | Pcstr_tuple parts ->
      gen_serialize_tuple_variant_impl ~ctxt ~typename ~variant_name ~idx parts
  | _ -> exit 0

let gen_serialize_variant_impl ~ctxt typename constructors =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let cases =
    List.mapi
      (fun idx ctr ->
        let lhs =
          Ast.ppat_construct ~loc
            (ctr.pcd_name.txt |> Longident.parse |> Loc.make ~loc)
            None
        in
        let rhs = gen_serialize_variant_ctr_impl ~ctxt ~typename idx ctr in
        Ast.case ~lhs ~rhs ~guard:None)
      constructors
  in
  Ast.pexp_match ~loc [%expr t] cases

let gen_serialize_impl ~ctxt type_decl =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let body =
    match type_decl with
    | { ptype_kind = Ptype_variant constructors; ptype_name; _ } ->
        gen_serialize_variant_impl ~ctxt ptype_name constructors
    | _ -> exit 0
  in
  [%stri
    (** Serialize a value of type `t` into Serde.data *)
    let serialize t =
      let ( let* ) = Result.bind in
      [%e body]]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  List.map (gen_serialize_impl ~ctxt) type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

(** interface *)

let generate_intf ~ctxt:_ (_rec_flag, _type_declarations) = exit 0
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

(** registration *)

let deriver =
  Deriving.add "serializer" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
