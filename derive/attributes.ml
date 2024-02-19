type type_attributes = {
  rename : string;
  mode :
    [ `tag of string
    | `tag_and_content of string * string
    | `untagged
    | `normal ];
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
  error_on_unknown_fields : bool;
}

type variant_attributes = {
  rename : string;
  should_skip : [ `skip_serializing | `skip_deserializing | `always | `never ];
  is_catch_all : bool;
}

type field_attributes = {
  rename : string;
  default : string option;
  should_skip :
    [ `skip_serializing_if of string
    | `skip_deserializing_if of string
    | `always
    | `never ];
}
