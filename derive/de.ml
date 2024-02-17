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

let deserializer_fn_name_for_longident name =
  let name =
    match name.txt |> Longident.flatten_exn |> List.rev with
    | name :: [] -> "deserialize_" ^ name
    | name :: path ->
        ("deserialize_" ^ name) :: path |> List.rev |> String.concat "."
    | _ -> "unknown"
  in
  Longident.parse name

let is_primitive = function
  | "bool" | "char" | "float" | "int" | "int32" | "int64" | "string" | "list"
  | "array" | "unit" | "option" ->
      true
  | _ -> false

let rec deserializer_for_type ~ctxt (core_type : Parsetree.core_type) =
  let loc = loc ~ctxt in
  match core_type.ptyp_desc with
  | Ptyp_constr (name, arg :: []) when is_primitive (Longident.name name.txt) ->
      let type_ser = deserializer_for_type ~ctxt arg in
      let name = Ast.pexp_ident ~loc name in
      [%expr d ([%e name] [%e type_ser])]
  | Ptyp_constr (name, []) when is_primitive (Longident.name name.txt) ->
      Ast.pexp_ident ~loc name
  | Ptyp_constr (name, _args) ->
      let ser_fn =
        deserializer_fn_name_for_longident name
        |> var ~ctxt |> Ast.pexp_ident ~loc
      in
      [%expr d [%e ser_fn]]
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

(** Deserializes records in different ways. *)
module Record_deserializer = struct
  (** Generates the implementation of a deserializer for a given record type (or
    list of label declarations).

    The outline of the generated code is:

      * create a field_visitor that maps strings and ints to an ad-hoc field
        polyvar (field name "hello" matches [`hello])

      * declare field value holders (one 'a option ref per field)

      * create a recursive function for consuimng fields one by one, using
        the visitor to validate them, and directing the deserialization to the
        right field deserializer

      * extract the field value holders and validate that all fields are present

      * construct the final result

  So for a record like:

  {ocaml[
    type person = { name: string; age: int }
  ]}

  The generated code would look like:

  {ocaml[
    let deserialize_person = De.(deserializer @@ fun ctx ->
      record ctx "person" 2 @@ fun ctx ->
        let field_visitor = Visitor.make
          ~visit_string:(fun _ctx str ->
            match str with
            | "name" -> Ok `name
            | "age" -> Ok `age
            | _ -> Error `invalid_field_type)
          ~visit_int:(fun _ctx int ->
            match int with
            | 0 -> Ok `name
            | 1 -> Ok `age
            | _ -> Error `invalid_field_type)
          ()
        in

        let name = ref None in
        let age = ref None in

        let rec read_fields () =
          let* tag = next_field ctx field_visitor in
          match tag with
          | Some `name ->
              let* v = field ctx "name" string in
              name := Some v;
              read_fields ()
          | Some `age ->
              let* v = field ctx "age" int in
              age := Some v;
              read_fields ()
          | None ->
              Ok ()
        in
        let* () = read_fields () in

        let* name = Option.to_result ~none:(`Msg "missing field 'name'") name in
        let* age = Option.to_result ~none:(`Msg "missing field 'age'") age in

        Ok {name;age}
    )
  ]}
*)
  let deserialize_with_unordered_fields ~ctxt labels final_expr =
    let loc = loc ~ctxt in
    let labels = List.rev labels in

    (* NOTE(@leostera): Generate the final assembling of the record value

       {ocaml[
         ... in
         Ok { name; age }
       ]}
    *)
    let record_expr =
      let fields =
        List.map
          (fun field ->
            let value = Ast.evar ~loc field.pld_name.txt in
            let field = Longident.parse field.pld_name.txt |> var ~ctxt in
            (field, value))
          labels
      in
      let record = Ast.pexp_record ~loc fields None in
      final_expr record
    in

    (* NOTE(@leostera): Generate the placeholder values for a list of fields:

       {ocaml[
         let name = ref None in
         let age = ref None in
         ...
       ]}
    *)
    let field_value_holders body =
      List.fold_left
        (fun last field ->
          let field = Ast.(pvar ~loc field.pld_name.txt) in
          [%expr
            let [%p field] = ref None in
            [%e last]])
        body labels
    in

    (* NOTE(@leostera): Generate the placeholder values for a list of fields:

       {ocaml[
         let name = ref None in
         let age = ref None in
         ...
       ]}
    *)
    let field_value_unwrapping body =
      List.fold_left
        (fun last field ->
          let field_var = Ast.(evar ~loc field.pld_name.txt) in
          let field_pat = Ast.(pvar ~loc field.pld_name.txt) in
          let missing_msg =
            Ast.estring ~loc
              (Format.sprintf "missing field %S" field.pld_name.txt)
          in
          [%expr
            let* [%p field_pat] =
              Option.to_result ~none:(`Msg [%e missing_msg]) ![%e field_var]
            in
            [%e last]])
        body labels
    in

    (* NOTE(@leostera): creates the visito from strings/ints to polymorphic
       variants for each field

       {ocaml[
          Visitor.make
            ~visit_string:(fun _ctx str ->
              match str with
              | "name" -> Ok `name
              | "age" -> Ok `age
              | _ -> Error `invalid_field_type)
            ~visit_int:(fun _ctx int ->
              match int with
              | 0 -> Ok `name
              | 1 -> Ok `age
              | _ -> Error `invalid_field_type)
            ()
       ]}
    *)
    let field_visitor next =
      let visit_string =
        let cases =
          List.map
            (fun field ->
              let lhs = Ast.pstring ~loc field.pld_name.txt in
              let rhs =
                let tag = Ast.pexp_variant ~loc field.pld_name.txt None in
                [%expr Ok [%e tag]]
              in
              Ast.case ~lhs ~rhs ~guard:None)
            labels
          @ [
              Ast.case ~lhs:(Ast.ppat_any ~loc) ~guard:None
                ~rhs:[%expr Error `invalid_tag];
            ]
        in
        let body = Ast.pexp_match ~loc [%expr str] cases in
        [%expr fun _ctx str -> [%e body]]
      in

      let visit_int =
        let cases =
          List.mapi
            (fun idx field ->
              let lhs = Ast.pint ~loc idx in
              let rhs =
                let tag = Ast.pexp_variant ~loc field.pld_name.txt None in
                [%expr Ok [%e tag]]
              in
              Ast.case ~lhs ~rhs ~guard:None)
            labels
          @ [
              Ast.case ~lhs:(Ast.ppat_any ~loc) ~guard:None
                ~rhs:[%expr Error `invalid_tag];
            ]
        in
        let body = Ast.pexp_match ~loc [%expr str] cases in
        [%expr fun _ctx str -> [%e body]]
      in

      [%expr
        let field_visitor =
          let visit_string = [%e visit_string] in
          let visit_int = [%e visit_int] in
          Visitor.make ~visit_string ~visit_int ()
        in
        [%e next]]
    in

    let declare_read_fields next =
      let cases =
        List.mapi
          (fun _idx (label : Parsetree.label_declaration) ->
            let lhs =
              let tag = Ast.ppat_variant ~loc label.pld_name.txt None in
              Ast.ppat_construct ~loc
                Longident.(parse "Some" |> var ~ctxt)
                (Some tag)
            in
            let rhs =
              let field_name = Ast.estring ~loc label.pld_name.txt in
              let field_var = Ast.(evar ~loc label.pld_name.txt) in
              let deserializer = deserializer_for_type ~ctxt label.pld_type in
              let assign =
                Ast.(
                  pexp_apply ~loc
                    (pexp_ident ~loc (var ~ctxt (Longident.parse ":=")))
                    [ (Nolabel, field_var); (Nolabel, [%expr Some v]) ])
              in
              [%expr
                let* v = field ctx [%e field_name] [%e deserializer] in
                [%e assign];
                read_fields ()]
            in
            Ast.case ~lhs ~guard:None ~rhs)
          labels
        @ [
            Ast.case
              ~lhs:
                (Ast.ppat_construct ~loc
                   Longident.(parse "None" |> var ~ctxt)
                   None)
              ~guard:None ~rhs:[%expr Ok ()];
          ]
      in

      let tag_match = Ast.pexp_match ~loc [%expr tag] cases in

      [%expr
        let rec read_fields () =
          let* tag = next_field ctx field_visitor in
          [%e tag_match]
        in
        [%e next]]
    in

    let call_read_fields next =
      [%expr
        let* () = read_fields () in
        [%e next]]
    in

    field_visitor
    (* declare all the optional references *)
    @@ field_value_holders
    (* declare our recursive function for consuming fields *)
    @@ declare_read_fields
    (* here's where the magic happens *)
    @@ call_read_fields
    (* unwrap all the boxes *)
    @@ field_value_unwrapping
    (* build the record *)
    @@ record_expr
end

let gen_serialize_variant_impl ~ctxt ptype_name cstr_declarations =
  let loc = loc ~ctxt in
  let type_name = Ast.estring ~loc ptype_name.txt in
  let constructor_names =
    Ast.elist ~loc
      (List.map
         (fun (cstr : Parsetree.constructor_declaration) ->
           Ast.estring ~loc cstr.pcd_name.txt)
         cstr_declarations)
  in

  let deser_by_constructor _type_name idx cstr =
    let _idx = Ast.eint ~loc idx in
    let name = Longident.parse cstr.pcd_name.txt |> var ~ctxt in
    match cstr.pcd_args with
    (* NOTE(@leostera): deserialize a single unit variant by calling
       `unit_variant` directly *)
    | Pcstr_tuple [] ->
        let value = Ast.pexp_construct ~loc name None in
        [%expr
          let* () = unit_variant ctx in
          Ok [%e value]]
    (* NOTE(@leostera): deserialize a newtype variant *)
    | Pcstr_tuple [ arg ] ->
        let sym = gensym () ~ctxt in
        let arg_pat = Ast.pvar ~loc sym.txt in
        let arg_var = Ast.evar ~loc sym.txt in

        let value =
          let cstr = Ast.pexp_construct ~loc name (Some arg_var) in
          [%expr Ok [%e cstr]]
        in

        let ser_fn = deserializer_for_type ~ctxt arg in
        let body =
          [%expr
            let* [%p arg_pat] = [%e ser_fn] ctx in
            [%e value]]
        in

        [%expr newtype_variant ctx @@ fun ctx -> [%e body]]
    (* NOTE(@leostera): deserialize a tuple variant *)
    | Pcstr_tuple args ->
        let gensym = gensym () in
        let arg_count = Ast.eint ~loc (List.length args) in
        let calls =
          List.mapi
            (fun _idx arg ->
              let ser_fn = deserializer_for_type ~ctxt arg in
              let arg_var = (gensym ~ctxt).txt in
              let deser =
                [%expr
                  match element ctx [%e ser_fn] with
                  | Ok (Some v) -> Ok v
                  | Ok None -> Error `no_more_data
                  | Error reason -> Error reason]
              in

              (arg_var, deser))
            args
        in

        let calls =
          let args =
            Ast.pexp_tuple ~loc
              (List.map (fun (field, _) -> Ast.evar ~loc field) calls)
          in
          let cstr = Ast.pexp_construct ~loc name (Some args) in

          List.fold_left
            (fun last (field, expr) ->
              let field = Ast.pvar ~loc field in
              [%expr
                let* [%p field] = [%e expr] in
                [%e last]])
            [%expr Ok [%e cstr]]
            (List.rev calls)
        in
        [%expr
          tuple_variant ctx [%e arg_count] (fun ~size ctx ->
              ignore size;
              [%e calls])]
    (* NOTE(@leostera): deserialize a record_variant *)
    | Pcstr_record labels ->
        let field_count = Ast.eint ~loc (List.length labels) in
        let body =
          Record_deserializer.deserialize_with_unordered_fields ~ctxt labels
          @@ fun record ->
          let cstr = Ast.pexp_construct ~loc name (Some record) in
          [%expr Ok [%e cstr]]
        in

        [%expr
          record_variant ctx [%e field_count] (fun ~size ctx ->
              ignore size;
              [%e body])]
  in

  let tag_dispatch =
    let cases =
      List.mapi
        (fun idx (cstr : Parsetree.constructor_declaration) ->
          let lhs = Ast.ppat_variant ~loc cstr.pcd_name.txt None in
          let rhs = deser_by_constructor type_name idx cstr in
          Ast.case ~lhs ~guard:None ~rhs)
        cstr_declarations
    in

    Ast.pexp_match ~loc [%expr tag] cases
  in

  let field_visitor =
    let cases =
      List.mapi
        (fun _idx (cstr : Parsetree.constructor_declaration) ->
          let tag_name = cstr.pcd_name.txt in
          let lhs = Ast.ppat_constant ~loc (Ast_helper.Const.string tag_name) in
          let rhs =
            let tag = Ast.pexp_variant ~loc cstr.pcd_name.txt None in
            [%expr Ok [%e tag]]
          in
          Ast.case ~lhs ~guard:None ~rhs)
        cstr_declarations
      @ [
          Ast.case ~lhs:(Ast.ppat_any ~loc) ~guard:None
            ~rhs:[%expr Error `invalid_tag];
        ]
    in

    let tag_match = Ast.pexp_match ~loc [%expr str] cases in

    [%expr Visitor.make ~visit_string:(fun _ctx str -> [%e tag_match]) ()]
  in

  [%expr
    let field_visitor = [%e field_visitor] in
    variant ctx [%e type_name] [%e constructor_names] @@ fun ctx ->
    let* tag = identifier ctx field_visitor in
    [%e tag_dispatch]]

let gen_serialize_record_impl ~ctxt ptype_name label_declarations =
  let loc = loc ~ctxt in
  let type_name = Ast.estring ~loc ptype_name.txt in
  let field_count = Ast.eint ~loc (List.length label_declarations) in

  let body =
    Record_deserializer.deserialize_with_unordered_fields ~ctxt
      label_declarations
    @@ fun record -> [%expr Ok [%e record]]
  in

  [%expr record ctx [%e type_name] [%e field_count] (fun ctx -> [%e body])]

let gen_serialize_impl ~ctxt type_decl =
  let loc = loc ~ctxt in

  let typename = type_decl.ptype_name.txt in

  let body =
    match type_decl with
    | { ptype_kind = Ptype_record label_declarations; ptype_name; _ } ->
        gen_serialize_record_impl ~ctxt ptype_name label_declarations
    | { ptype_kind = Ptype_variant cstrs_declaration; ptype_name; _ } ->
        gen_serialize_variant_impl ~ctxt ptype_name cstrs_declaration
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
  let deserializer_name =
    "deserialize_" ^ typename |> var ~ctxt |> Ast.ppat_var ~loc
  in
  [%stri
    let [%p deserializer_name] =
      let ( let* ) = Result.bind in
      Serde.De.(fun ctx -> [%e body])]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = loc ~ctxt in
  [ [%stri open! Serde]; [%stri let ( let* ) = Result.bind] ]
  @ List.map (gen_serialize_impl ~ctxt) type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

(** interface *)

let generate_intf ~ctxt:_ (_rec_flag, _type_declarations) = []
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

(** registration *)

let register =
  Deriving.add "deserialize" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
