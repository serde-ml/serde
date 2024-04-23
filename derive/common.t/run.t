  $ dune clean
  $ dune exec ./common_test.exe | jq .
  [
    [
      [
        1,
        2,
        3,
        4
      ],
      [
        4,
        3,
        2,
        1
      ]
    ],
    [
      [
        1,
        2,
        3,
        4
      ],
      [
        4,
        3,
        2,
        1
      ]
    ]
  ]
  $ dune describe pp ./common_test.ml
  [@@@ocaml.ppx.context
    {
      tool_name = "ppx_driver";
      include_dirs = [];
      load_path = [];
      open_modules = [];
      for_package = None;
      debug = false;
      use_threads = false;
      use_vmthreads = false;
      recursive_types = false;
      principal = false;
      transparent_modules = false;
      unboxed_types = false;
      unsafe_string = false;
      cookies = []
    }]
  type numbers = int list[@@deriving (serialize, deserialize)]
  include
    struct
      let _ = fun (_ : numbers) -> ()
      let ( let* ) = Result.bind
      let _ = ( let* )
      let serialize_numbers =
        let open Serde.Ser in fun t -> fun ctx -> (s (list int)) t ctx
      let _ = serialize_numbers
      open! Serde
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_numbers =
        let ( let* ) = Result.bind in
        let _ = ( let* ) in let open Serde.De in fun ctx -> (d (list int)) ctx
      let _ = deserialize_numbers
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type number_list_list = numbers array[@@deriving (serialize, deserialize)]
  include
    struct
      let _ = fun (_ : number_list_list) -> ()
      let ( let* ) = Result.bind
      let _ = ( let* )
      let serialize_number_list_list =
        let open Serde.Ser in
          fun t -> fun ctx -> (s (array (s serialize_numbers))) t ctx
      let _ = serialize_number_list_list
      open! Serde
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_number_list_list =
        let ( let* ) = Result.bind in
        let _ = ( let* ) in
        let open Serde.De in fun ctx -> (d (array (d deserialize_numbers))) ctx
      let _ = deserialize_number_list_list
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () =
    let test_t = [|[1; 2; 3; 4];[4; 3; 2; 1]|] in
    let json1 =
      (Serde_json.to_string serialize_number_list_list test_t) |> Result.get_ok in
    let value =
      (Serde_json.of_string deserialize_number_list_list json1) |>
        Result.get_ok in
    let json2 =
      (Serde_json.to_string serialize_number_list_list value) |> Result.get_ok in
    Format.printf "[%s,%s]\n%!" json1 json2
