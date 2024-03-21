open Ppxlib

type type_attributes = {
  rename : string;
  variant_tagging_mode :
    [ `externally_tagged
    | `internally_tagged of string
    | `adjacently_tagged of string * string
    | `untagged ];
  rename_all :
    [ `lowercase
    | `UPPERCASE
    | `camelCase
    | `PascalCase
    | `snake_case
    | `SCREAMING_SNAKE_CASE
    | `kebab_case
    | `SCREAMING_KEBAB_CASE ]
    option;
  deny_unknown_fields : bool;
}

type variant_attributes = {
  rename : string;
  should_skip : [ `skip_serializing | `skip_deserializing | `always | `never ];
  is_catch_all : bool;
}

type field_attributes = {
  name : string;
  presence : [ `required | `optional | `with_default of Parsetree.expression ];
  should_skip :
    [ `skip_serializing_if of string
    | `skip_deserializing_if of string
    | `always
    | `never ];
}

(* deserialize_ignored_any *)
let of_record_attributes attributes =
  (* Field defaults *)
  let rename_all = ref None in
  let deny_unknown_fields = ref false in
  let variant_tagging_mode = ref `externally_tagged in
  (* Retrieve fields *)
  let serde_attr =
    List.find_opt (fun attr -> attr.attr_name.txt = "serde") attributes
  in
  Option.iter
    (fun attr ->
      match attr.attr_payload with
      | PStr
          [
            {
              pstr_desc =
                Pstr_eval ({ pexp_desc = Pexp_record (fields, _); _ }, _);
              _;
            };
          ] ->
          List.iter
            (function
              | ( { txt = Lident "tag"; _ },
                  { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } )
                ->
                  variant_tagging_mode := `internally_tagged s
              | ( { txt = Lident "content"; _ },
                  { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } )
                ->
                  let new_mode =
                    match !variant_tagging_mode with
                    | `internally_tagged t -> `adjacently_tagged (t, s)
                    | _ ->
                        failwith
                          "[ppx_serde] You can only use the 'content' \
                           attribute after the 'tag' attribute."
                  in
                  variant_tagging_mode := new_mode
              | { txt = Lident "untagged"; _ }, [%expr true] ->
                  variant_tagging_mode := `untagged
              | { txt = Lident "deny_unknown_fields"; _ }, [%expr true] ->
                  deny_unknown_fields := true
              | { txt = Lident "deny_unknown_fields"; _ }, [%expr false] ->
                  deny_unknown_fields := false
              | { txt = Lident "rename_all"; _ }, expr ->
                  let rename_value =
                    match expr with
                    | [%expr "none"] -> None
                    | [%expr "lowercase"] -> Some `lowercase
                    | [%expr "UPPERCASE"] -> Some `UPPERCASE
                    | [%expr "kebab-case"] -> Some `kebab_case
                    | [%expr "camelCase"] -> Some `camelCase
                    | [%expr "bestCase"] -> Some `camelCase
                    | [%expr "PascalCase"] -> Some `PascalCase
                    | [%expr "snake_case"] -> Some `snake_case
                    | [%expr "SCREAMING_SNAKE_CASE"] ->
                        Some `SCREAMING_SNAKE_CASE
                    | [%expr "SCREAMING-KEBAB-CASE"] ->
                        Some `SCREAMING_KEBAB_CASE
                    | _ ->
                        failwith
                          (Format.asprintf
                             "[ppx_serde] Unknown rename_all value '%a'"
                             Pprintast.expression expr)
                  in
                  rename_all := rename_value
              | { txt = Lident txt; _ }, _ ->
                  failwith
                    (Format.sprintf "[ppx_serde] Unknown attribute %S" txt)
              | _ -> ())
            fields
      | _ -> ())
    serde_attr;
  {
    rename = "";
    variant_tagging_mode = !variant_tagging_mode;
    rename_all = !rename_all;
    deny_unknown_fields = !deny_unknown_fields;
  }

let pascal_case field =
  let is_underscore c = c = '_' in
  let to_uppercase c = Char.uppercase_ascii c in
  let rec aux chars capitalize acc =
    match chars with
    | [] -> List.rev acc
    | c :: cs when is_underscore c -> aux cs true acc
    | c :: cs when capitalize -> aux cs false (to_uppercase c :: acc)
    | c :: cs -> aux cs capitalize (c :: acc)
  in
  let chars = String.to_seq field |> List.of_seq in
  let pascal_list = aux chars true [] in
  String.of_seq (List.to_seq pascal_list)

let kebab_case field = String.map (function '_' -> '-' | c -> c) field

let of_field_attributes type_attributes lbl =
  let open Ppxlib in
  let name =
    ref
      (match type_attributes.rename_all with
      | Some `lowercase -> String.lowercase_ascii lbl.pld_name.txt
      | Some `UPPERCASE -> String.uppercase_ascii lbl.pld_name.txt
      | Some `PascalCase -> pascal_case lbl.pld_name.txt
      | Some `camelCase ->
          let pascal = pascal_case lbl.pld_name.txt in
          let start = String.sub pascal 0 1 |> String.lowercase_ascii in
          let rest = String.sub pascal 1 (String.length pascal - 1) in
          start ^ rest
      | Some `snake_case -> lbl.pld_name.txt
      | Some `kebab_case -> kebab_case lbl.pld_name.txt
      | Some `SCREAMING_SNAKE_CASE -> String.uppercase_ascii lbl.pld_name.txt
      | Some `SCREAMING_KEBAB_CASE ->
          kebab_case lbl.pld_name.txt |> String.uppercase_ascii
      | None -> lbl.pld_name.txt)
  in
  let should_skip = ref `never in
  let presence =
    ref
      (match lbl.pld_type.ptyp_desc with
      | Ptyp_constr ({ txt = Lident "option"; _ }, _) -> `optional
      | _ -> `required)
  in
  let () =
    match lbl.pld_attributes with
    | [
     {
       attr_name = { txt = "serde"; _ };
       attr_payload =
         PStr
           [
             {
               pstr_desc =
                 Pstr_eval ({ pexp_desc = Pexp_record (fields, _); _ }, _);
               _;
             };
           ];
       _;
     };
    ] ->
        List.iter
          (fun (label, expr) ->
            match (label, expr) with
            | ( { txt = Lident "rename"; _ },
                { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } ) ->
                name := s;
                ()
            | { txt = Lident "default"; _ }, expr ->
                presence := `with_default expr;
                ()
            | _ -> ())
          fields
    | _ -> ()
  in

  (lbl, { name = !name; presence = !presence; should_skip = !should_skip })
