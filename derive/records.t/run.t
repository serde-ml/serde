  $ dune clean
  $ dune exec ./record_test.exe | jq .
  [
    {
      "stuff": [
        {
          "name": "hello",
          "commisioned": false,
          "updated_at": 9223372036854766,
          "credits": null,
          "keywords": [],
          "rank": {
            "rank_scores": [
              "1",
              "c",
              "a"
            ],
            "rank_name": "asdf"
          },
          "value": 420.69,
          "type": "something"
        },
        {
          "name": "hello",
          "commisioned": false,
          "updated_at": 0,
          "credits": 2112,
          "keywords": [
            "hello"
          ],
          "rank": {
            "rank_scores": [],
            "rank_name": "asdf"
          },
          "value": 3.14159265359,
          "type": "something"
        }
      ]
    },
    {
      "stuff": [
        {
          "name": "hello",
          "commisioned": false,
          "updated_at": 9223372036854766,
          "credits": null,
          "keywords": [],
          "rank": {
            "rank_scores": [
              "1",
              "c",
              "a"
            ],
            "rank_name": "asdf"
          },
          "value": 420.69,
          "type": "something"
        },
        {
          "name": "hello",
          "commisioned": false,
          "updated_at": 0,
          "credits": 2112,
          "keywords": [
            "hello"
          ],
          "rank": {
            "rank_scores": [],
            "rank_name": "asdf"
          },
          "value": 3.14159265359,
          "type": "something"
        }
      ]
    }
  ]
  $ dune describe pp ./record_test.ml
  [@@@ocaml.ppx.context
    {
      tool_name = "ppx_driver";
      include_dirs = [];
      hidden_include_dirs = [];
      load_path = ([], []);
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
  type rank = {
    rank_scores: string list ;
    rank_name: string }[@@deriving (serialize, deserialize)]
  include
    struct
      let _ = fun (_ : rank) -> ()
      let serialize_rank =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
          fun t ctx ->
            record ctx "rank" 2
              (fun ctx ->
                 let* () =
                   field ctx "rank_scores" ((s (list string)) t.rank_scores)
                  in
                 let* () = field ctx "rank_name" (string t.rank_name)
                  in Ok ())
      let _ = serialize_rank
      open! Serde
      let deserialize_rank =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.De in
          fun ctx ->
            record ctx "rank" 2
              (fun ctx ->
                 let field_visitor =
                   let visit_string _ctx str =
                     match str with
                     | "rank_name" -> Ok `rank_name
                     | "rank_scores" -> Ok `rank_scores
                     | _ -> Ok `invalid_tag in
                   let visit_int _ctx str =
                     match str with
                     | 0 -> Ok `rank_name
                     | 1 -> Ok `rank_scores
                     | _ -> Ok `invalid_tag in
                   Visitor.make ~visit_string ~visit_int () in
                 let rank_scores = ref None in
                 let rank_name = ref None in
                 let rec read_fields () =
                   let* tag = next_field ctx field_visitor
                    in
                   match tag with
                   | Some `rank_name ->
                       let* v = field ctx "rank_name" string
                        in (rank_name := (Some v); read_fields ())
                   | Some `rank_scores ->
                       let* v = field ctx "rank_scores" (d (list string))
                        in (rank_scores := (Some v); read_fields ())
                   | Some `invalid_tag ->
                       let* () = ignore_any ctx
                        in read_fields ()
                   | None -> Ok () in
                 let* () = read_fields ()
                  in
                 let* rank_scores =
                   Stdlib.Option.to_result
                     ~none:(`Msg
                              "missing field \"rank_scores\" (\"rank_scores\")")
                     (!rank_scores)
                  in
                 let* rank_name =
                   Stdlib.Option.to_result
                     ~none:(`Msg "missing field \"rank_name\" (\"rank_name\")")
                     (!rank_name)
                  in Ok { rank_name; rank_scores })
      let _ = deserialize_rank
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type t =
    {
    name: string ;
    commisioned: bool ;
    updated_at: int64 ;
    credits: int32 option ;
    keywords: string array ;
    rank: rank ;
    value: float ;
    type_: string [@serde { rename = "type" }]}[@@deriving
                                                 (serialize, deserialize)]
  include
    struct
      let _ = fun (_ : t) -> ()
      let serialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
          fun t ctx ->
            record ctx "t" 8
              (fun ctx ->
                 let* () = field ctx "name" (string t.name)
                  in
                 let* () = field ctx "commisioned" (bool t.commisioned)
                  in
                 let* () = field ctx "updated_at" (int64 t.updated_at)
                  in
                 let* () = field ctx "credits" ((s (option int32)) t.credits)
                  in
                 let* () = field ctx "keywords" ((s (array string)) t.keywords)
                  in
                 let* () = field ctx "rank" ((s serialize_rank) t.rank)
                  in
                 let* () = field ctx "value" (float t.value)
                  in let* () = field ctx "type" (string t.type_)
                      in Ok ())
      let _ = serialize_t
      open! Serde
      let deserialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.De in
          fun ctx ->
            record ctx "t" 8
              (fun ctx ->
                 let field_visitor =
                   let visit_string _ctx str =
                     match str with
                     | "type" -> Ok `type_
                     | "value" -> Ok `value
                     | "rank" -> Ok `rank
                     | "keywords" -> Ok `keywords
                     | "credits" -> Ok `credits
                     | "updated_at" -> Ok `updated_at
                     | "commisioned" -> Ok `commisioned
                     | "name" -> Ok `name
                     | _ -> Ok `invalid_tag in
                   let visit_int _ctx str =
                     match str with
                     | 0 -> Ok `type_
                     | 1 -> Ok `value
                     | 2 -> Ok `rank
                     | 3 -> Ok `keywords
                     | 4 -> Ok `credits
                     | 5 -> Ok `updated_at
                     | 6 -> Ok `commisioned
                     | 7 -> Ok `name
                     | _ -> Ok `invalid_tag in
                   Visitor.make ~visit_string ~visit_int () in
                 let name = ref None in
                 let commisioned = ref None in
                 let updated_at = ref None in
                 let credits = ref None in
                 let keywords = ref None in
                 let rank = ref None in
                 let value = ref None in
                 let type_ = ref None in
                 let rec read_fields () =
                   let* tag = next_field ctx field_visitor
                    in
                   match tag with
                   | Some `type_ ->
                       let* v = field ctx "type" string
                        in (type_ := (Some v); read_fields ())
                   | Some `value ->
                       let* v = field ctx "value" float
                        in (value := (Some v); read_fields ())
                   | Some `rank ->
                       let* v = field ctx "rank" (d deserialize_rank)
                        in (rank := (Some v); read_fields ())
                   | Some `keywords ->
                       let* v = field ctx "keywords" (d (array string))
                        in (keywords := (Some v); read_fields ())
                   | Some `credits ->
                       let* v = field ctx "credits" (d (option int32))
                        in (credits := (Some v); read_fields ())
                   | Some `updated_at ->
                       let* v = field ctx "updated_at" int64
                        in (updated_at := (Some v); read_fields ())
                   | Some `commisioned ->
                       let* v = field ctx "commisioned" bool
                        in (commisioned := (Some v); read_fields ())
                   | Some `name ->
                       let* v = field ctx "name" string
                        in (name := (Some v); read_fields ())
                   | Some `invalid_tag ->
                       let* () = ignore_any ctx
                        in read_fields ()
                   | None -> Ok () in
                 let* () = read_fields ()
                  in
                 let* name =
                   Stdlib.Option.to_result
                     ~none:(`Msg "missing field \"name\" (\"name\")") (
                     !name)
                  in
                 let* commisioned =
                   Stdlib.Option.to_result
                     ~none:(`Msg
                              "missing field \"commisioned\" (\"commisioned\")")
                     (!commisioned)
                  in
                 let* updated_at =
                   Stdlib.Option.to_result
                     ~none:(`Msg
                              "missing field \"updated_at\" (\"updated_at\")")
                     (!updated_at)
                  in
                 let credits =
                   match !credits with | Some opt -> opt | None -> None in
                 let* keywords =
                   Stdlib.Option.to_result
                     ~none:(`Msg "missing field \"keywords\" (\"keywords\")")
                     (!keywords)
                  in
                 let* rank =
                   Stdlib.Option.to_result
                     ~none:(`Msg "missing field \"rank\" (\"rank\")") (
                     !rank)
                  in
                 let* value =
                   Stdlib.Option.to_result
                     ~none:(`Msg "missing field \"value\" (\"value\")")
                     (!value)
                  in
                 let* type_ =
                   Stdlib.Option.to_result
                     ~none:(`Msg "missing field \"type\" (\"type_\")") (
                     !type_)
                  in
                 Ok
                   {
                     type_;
                     value;
                     rank;
                     keywords;
                     credits;
                     updated_at;
                     commisioned;
                     name
                   })
      let _ = deserialize_t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type t_list = {
    stuff: t list }[@@deriving (serialize, deserialize)]
  include
    struct
      let _ = fun (_ : t_list) -> ()
      let serialize_t_list =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
          fun t ctx ->
            record ctx "t_list" 1
              (fun ctx ->
                 let* () =
                   field ctx "stuff" ((s (list (s serialize_t))) t.stuff)
                  in Ok ())
      let _ = serialize_t_list
      open! Serde
      let deserialize_t_list =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.De in
          fun ctx ->
            record ctx "t_list" 1
              (fun ctx ->
                 let field_visitor =
                   let visit_string _ctx str =
                     match str with
                     | "stuff" -> Ok `stuff
                     | _ -> Ok `invalid_tag in
                   let visit_int _ctx str =
                     match str with | 0 -> Ok `stuff | _ -> Ok `invalid_tag in
                   Visitor.make ~visit_string ~visit_int () in
                 let stuff = ref None in
                 let rec read_fields () =
                   let* tag = next_field ctx field_visitor
                    in
                   match tag with
                   | Some `stuff ->
                       let* v = field ctx "stuff" (d (list (d deserialize_t)))
                        in (stuff := (Some v); read_fields ())
                   | Some `invalid_tag ->
                       let* () = ignore_any ctx
                        in read_fields ()
                   | None -> Ok () in
                 let* () = read_fields ()
                  in
                 let* stuff =
                   Stdlib.Option.to_result
                     ~none:(`Msg "missing field \"stuff\" (\"stuff\")")
                     (!stuff)
                  in Ok { stuff })
      let _ = deserialize_t_list
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () =
    let test_t =
      {
        stuff =
          [{
             name = "hello";
             commisioned = false;
             updated_at = 9223372036854766L;
             credits = None;
             keywords = [||];
             rank = { rank_name = "asdf"; rank_scores = ["1"; "c"; "a"] };
             value = 420.69;
             type_ = "something"
           };
          {
            name = "hello";
            commisioned = false;
            updated_at = 0L;
            credits = (Some 2112l);
            keywords = [|"hello"|];
            rank = { rank_name = "asdf"; rank_scores = [] };
            value = Float.pi;
            type_ = "something"
          }]
      } in
    let json1 = (Serde_json.to_string serialize_t_list test_t) |> Result.get_ok in
    let value =
      (Serde_json.of_string deserialize_t_list json1) |> Result.get_ok in
    let json2 = (Serde_json.to_string serialize_t_list value) |> Result.get_ok in
    Format.printf "[%s,%s]\n%!" json1 json2
