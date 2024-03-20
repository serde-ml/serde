type with_lowercase = { tEsTinG : bool }
[@@deriving serialize, deserialize] [@@serde { rename_all = "lowercase" }]

let ( let* ) = Result.bind

let test_de_with_lowercase () =
  let input = {| { "testing": true } |} in
  match Serde_json.of_string deserialize_with_lowercase input with
  | Ok x ->
      assert (x.tEsTinG = true);
      Format.printf "serde_json.ser/de test \"parsed: with_lowercase\"@."
  | Error _ -> failwith "Could not parse"

let test_ser_with_lowercase () =
  let input = {|{"testing":true}|} in
  let* parsed = Serde_json.of_string deserialize_with_lowercase input in
  let* serialized = Serde_json.to_string serialize_with_lowercase parsed in
  assert (input = serialized);
  Format.printf "serde_json.ser/de test \"serialized: with_lowercase\"@.";
  Ok ()

type with_uppercase = { tEsTinG : bool }
[@@deriving serialize, deserialize] [@@serde { rename_all = "UPPERCASE" }]

let test_de_with_uppercase () =
  let input = {| { "TESTING": true } |} in
  match Serde_json.of_string deserialize_with_uppercase input with
  | Ok x ->
      assert (x.tEsTinG = true);
      Format.printf "serde_json.ser/de test \"parsed: with_uppercase\"@."
  | Error _ -> failwith "Could not parse"

let test_ser_with_uppercase () =
  let input = {|{"TESTING":true}|} in
  let* parsed = Serde_json.of_string deserialize_with_uppercase input in
  let* serialized = Serde_json.to_string serialize_with_uppercase parsed in
  assert (input = serialized);
  Format.printf "serde_json.ser/de test \"serialized: with_uppercase\"@.";
  Ok ()

type with_snakecase = { testing_field : bool }
[@@deriving serialize, deserialize] [@@serde { rename_all = "snake_case" }]

let test_de_with_snakecase () =
  let input = {| { "testing_field": true } |} in
  match Serde_json.of_string deserialize_with_snakecase input with
  | Ok x ->
      assert (x.testing_field = true);
      Format.printf "serde_json.ser/de test \"parsed: with_snakecase\"@."
  | Error _ -> failwith "Could not parse"

type with_camelcase = { camel_case : bool }
[@@deriving serialize, deserialize] [@@serde { rename_all = "camelCase" }]

let test_de_with_camelcase () =
  let input = {| { "camelCase": true } |} in
  match Serde_json.of_string deserialize_with_camelcase input with
  | Ok x ->
      assert (x.camel_case = true);
      Format.printf "serde_json.ser/de test \"parsed: with_camelcase\"@."
  | Error _ -> failwith "Could not parse: with_camelcase"

let test_ser_with_camelcase () =
  let input = {|{"camelCase":true}|} in
  let* parsed = Serde_json.of_string deserialize_with_camelcase input in
  let* serialized = Serde_json.to_string serialize_with_camelcase parsed in
  assert (input = serialized);
  Format.printf "serde_json.ser/de test \"serialized: with_camelCase\"@.";
  Ok ()

type with_pascalcase = { pascal_case : bool }
[@@deriving serialize, deserialize] [@@serde { rename_all = "PascalCase" }]

let test_de_with_pascalcase () =
  let input = {| { "PascalCase": true } |} in
  match Serde_json.of_string deserialize_with_pascalcase input with
  | Ok x ->
      assert (x.pascal_case = true);
      Format.printf "serde_json.ser/de test \"parsed: with_pascalcase\"@."
  | Error _ -> failwith "Could not parse: with_pascalcase"

type with_kebabcase = { kebab_case : bool }
[@@deriving serialize, deserialize] [@@serde { rename_all = "kebab-case" }]

let test_de_with_kebabcase () =
  let input = {| { "kebab-case": true } |} in
  match Serde_json.of_string deserialize_with_kebabcase input with
  | Ok x ->
      assert (x.kebab_case = true);
      Format.printf "serde_json.ser/de test \"parsed: with_kebabcase\"@."
  | Error _ -> failwith "Could not parse: with_kebabcase"

type with_screamingkebabcase = { kebab_case : bool }
[@@deriving serialize, deserialize]
[@@serde { rename_all = "SCREAMING-KEBAB-CASE" }]

let test_de_with_screamingkebabcase () =
  let input = {| { "KEBAB-CASE": true } |} in
  match Serde_json.of_string deserialize_with_screamingkebabcase input with
  | Ok x ->
      assert (x.kebab_case = true);
      Format.printf
        "serde_json.ser/de test \"parsed: with_screamingkebabcase\"@."
  | Error _ -> failwith "Could not parse: with_screamingkebabcase"

type with_screamingsnakecase = { snake_case : bool }
[@@deriving serialize, deserialize]
[@@serde { rename_all = "SCREAMING_SNAKE_CASE" }]

let test_de_with_screamingsnakecase () =
  let input = {| { "SNAKE_CASE": true } |} in
  match Serde_json.of_string deserialize_with_screamingsnakecase input with
  | Ok x ->
      assert (x.snake_case = true);
      Format.printf
        "serde_json.ser/de test \"parsed: with_screamingsnakecase\"@."
  | Error _ -> failwith "Could not parse: with_screamingsnakecase"

let () =
  test_de_with_lowercase ();
  test_de_with_uppercase ();
  test_de_with_snakecase ();
  test_de_with_camelcase ();
  test_de_with_pascalcase ();
  test_de_with_kebabcase ();
  test_de_with_screamingkebabcase ();
  test_de_with_screamingsnakecase ();

  test_ser_with_lowercase () |> Result.get_ok;
  test_ser_with_uppercase () |> Result.get_ok;
  test_ser_with_camelcase () |> Result.get_ok
