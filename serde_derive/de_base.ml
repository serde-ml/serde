open Ppxlib
module Ast = Ast_builder.Default

(** helpers *)
let loc ~ctxt = Expansion_context.Deriver.derived_item_loc ctxt

let var ~ctxt name =
  let loc = loc ~ctxt in
  Loc.make ~loc name

let longident ~ctxt name = name |> Longident.parse |> var ~ctxt

let rec is_primitive_type (t : core_type) =
  match t.ptyp_desc with
  | Ptyp_constr (name, [ tp ]) -> (
      match name.txt |> Longident.name with
      | "option" -> is_primitive_type tp
      | _ -> false)
  | Ptyp_constr (name, _) -> (
      match name.txt |> Longident.name with
      | "bool" | "char" | "float" | "int" | "string" | "unit" -> true
      | _ -> false)
  | _ -> false

(** visitor / deserializer resolution *)

let rec de_fun ~ctxt (t : core_type) =
  let loc = loc ~ctxt in
  match t.ptyp_desc with
  (* Serialize a constructor *)
  | Ptyp_constr (name, [ tp ]) -> (
      match name.txt |> Longident.name with
      | "option" ->
          let e = de_fun ~ctxt tp in
          [%expr Serde.De.deserialize_option [%e e]]
      | _ -> [%expr ()])
  | Ptyp_constr (name, _) -> (
      match name.txt |> Longident.name with
      | "bool" -> [%expr Serde.De.deserialize_bool]
      | "char" -> [%expr Serde.De.deserialize_char]
      | "float" -> [%expr Serde.De.deserialize_float]
      | "int" -> [%expr Serde.De.deserialize_int]
      | "string" -> [%expr Serde.De.deserialize_string]
      | "unit" -> [%expr Serde.De.deserialize_unit]
      | _ ->
          let ser_fn_name =
            match name.txt |> Longident.flatten_exn |> List.rev with
            | name :: [] -> "deserialize_" ^ name
            | name :: path ->
                ("deserialize_" ^ name) :: path |> List.rev |> String.concat "."
            | _ -> "unknown"
          in
          Ast.pexp_ident ~loc (longident ~ctxt ser_fn_name))
  | Ptyp_alias (_, _) ->
      Printf.printf "found alias";
      [%expr ()]
  (* Unsupported serialization for these *)
  | Ptyp_tuple _ ->
      Printf.printf "found tuple";
      [%expr ()]
  | Ptyp_any ->
      Printf.printf "found any";
      [%expr ()]
  | Ptyp_var _ ->
      Printf.printf "found var";
      [%expr ()]
  | Ptyp_object (_, _) ->
      Printf.printf "found object";
      [%expr ()]
  | Ptyp_class (_, _) ->
      Printf.printf "found class";
      [%expr ()]
  | Ptyp_variant (_, _, _) ->
      Printf.printf "found variant";
      [%expr ()]
  | Ptyp_poly (_, _) ->
      Printf.printf "found poly";
      [%expr ()]
  | Ptyp_package _ ->
      Printf.printf "found package";
      [%expr ()]
  | Ptyp_extension _ ->
      Printf.printf "found extension";
      [%expr ()]
  | Ptyp_arrow (_, _, _) ->
      Printf.printf "found arrow";
      [%expr ()]

let rec visitor_mod ~ctxt (t : core_type) =
  let loc = loc ~ctxt in
  match t.ptyp_desc with
  (* Serialize a constructor *)
  | Ptyp_constr (name, [ tp ]) -> (
      match name.txt |> Longident.name with
      | "option" -> visitor_mod ~ctxt tp
      | _ -> None)
  | Ptyp_constr (name, _) -> (
      match name.txt |> Longident.name with
      | "bool" -> Some [%expr (module Serde.De.Impls.Bool_visitor)]
      | "char" -> Some [%expr (module Serde.De.Impls.Char_visitor)]
      | "float" -> Some [%expr (module Serde.De.Impls.Float_visitor)]
      | "int" -> Some [%expr (module Serde.De.Impls.Int_visitor)]
      | "string" -> Some [%expr (module Serde.De.Impls.String_visitor)]
      | "unit" -> Some [%expr (module Serde.De.Impls.Unit_visitor)]
      | _ -> None)
  (* Unsupported serialization for these *)
  | Ptyp_tuple _ | Ptyp_any | Ptyp_var _
  | Ptyp_object (_, _)
  | Ptyp_class (_, _)
  | Ptyp_alias (_, _)
  | Ptyp_variant (_, _, _)
  | Ptyp_poly (_, _)
  | Ptyp_package _ | Ptyp_extension _
  | Ptyp_arrow (_, _, _) ->
      None
