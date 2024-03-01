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
  type rank = {
    rank_scores: string list ;
    rank_name: string }[@@deriving (serialize, deserialize)]
  include
    struct
      let _ = fun (_ : rank) -> ()
      let ( let* ) = Result.bind
      let _ = ( let* )
      let serialize_rank =
        let open Serde.Ser in
          fun t ->
            fun ctx ->
              record ctx "rank" 2
                (fun ctx ->
                   let* () =
                     field ctx "rank_scores" ((s (list string)) t.rank_scores)
                    in
                   let* () = field ctx "rank_name" (string t.rank_name)
                    in Ok ())
      let _ = serialize_rank
      open! Serde
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_rank =
        let ( let* ) = Result.bind in
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
                   Option.to_result
                     ~none:(`Msg
                              "missing field \"rank_scores\" (\"rank_scores\")")
                     (!rank_scores)
                  in
                 let* rank_name =
                   Option.to_result
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
      let ( let* ) = Result.bind
      let _ = ( let* )
      let serialize_t =
        let open Serde.Ser in
          fun t ->
            fun ctx ->
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
                   let* () =
                     field ctx "keywords" ((s (array string)) t.keywords)
                    in
                   let* () = field ctx "rank" ((s serialize_rank) t.rank)
                    in
                   let* () = field ctx "value" (float t.value)
                    in let* () = field ctx "type" (string t.type_)
                        in Ok ())
      let _ = serialize_t
      open! Serde
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_t =
        let ( let* ) = Result.bind in
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
                   Option.to_result
                     ~none:(`Msg "missing field \"name\" (\"name\")") (
                     !name)
                  in
                 let* commisioned =
                   Option.to_result
                     ~none:(`Msg
                              "missing field \"commisioned\" (\"commisioned\")")
                     (!commisioned)
                  in
                 let* updated_at =
                   Option.to_result
                     ~none:(`Msg
                              "missing field \"updated_at\" (\"updated_at\")")
                     (!updated_at)
                  in
                 let credits =
                   match !credits with | Some opt -> opt | None -> None in
                 let* keywords =
                   Option.to_result
                     ~none:(`Msg "missing field \"keywords\" (\"keywords\")")
                     (!keywords)
                  in
                 let* rank =
                   Option.to_result
                     ~none:(`Msg "missing field \"rank\" (\"rank\")") (
                     !rank)
                  in
                 let* value =
                   Option.to_result
                     ~none:(`Msg "missing field \"value\" (\"value\")")
                     (!value)
                  in
                 let* type_ =
                   Option.to_result
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
      let ( let* ) = Result.bind
      let _ = ( let* )
      let serialize_t_list =
        let open Serde.Ser in
          fun t ->
            fun ctx ->
              record ctx "t_list" 1
                (fun ctx ->
                   let* () =
                     field ctx "stuff" ((s (list (s serialize_t))) t.stuff)
                    in Ok ())
      let _ = serialize_t_list
      open! Serde
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_t_list =
        let ( let* ) = Result.bind in
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
                   Option.to_result
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




Now we test the variants:

  $ dune exec ./variant_test.exe | jq .
  [
    {
      "Ranks": [
        "Ensign",
        {
          "Commander": [
            "riker",
            2112,
            3.14159265359
          ]
        },
        {
          "Lt": null
        },
        {
          "Lt": false
        },
        {
          "Lt": true
        },
        {
          "Captain": {
            "name": "janeway",
            "ship": "voyager"
          }
        }
      ]
    },
    {
      "Ranks": [
        "Ensign",
        {
          "Commander": [
            "riker",
            2112,
            3.14159265359
          ]
        },
        {
          "Lt": null
        },
        {
          "Lt": false
        },
        {
          "Lt": true
        },
        {
          "Captain": {
            "name": "janeway",
            "ship": "voyager"
          }
        }
      ]
    }
  ]
  $ dune describe pp ./variant_test.ml
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
  [@@@warning "-37"]
  type rank =
    | Captain of {
    name: string ;
    ship: string } 
    | Commander of string * int32 * float 
    | Lt of bool option 
    | Ensign [@@deriving (serialize, deserialize)]
  include
    struct
      let _ = fun (_ : rank) -> ()
      let ( let* ) = Result.bind
      let _ = ( let* )
      let serialize_rank =
        let open Serde.Ser in
          fun t ->
            fun ctx ->
              match t with
              | Captain r ->
                  record_variant ctx "rank" 0 "Captain" 2
                    (fun ctx ->
                       let* () = field ctx "name" (string r.name)
                        in let* () = field ctx "ship" (string r.ship)
                            in Ok ())
              | Commander (v_1, v_2, v_3) ->
                  tuple_variant ctx "rank" 1 "Commander" 3
                    (fun ctx ->
                       let* () = element ctx (string v_1)
                        in
                       let* () = element ctx (int32 v_2)
                        in let* () = element ctx (float v_3)
                            in Ok ())
              | Lt v_1 ->
                  newtype_variant ctx "rank" 2 "Lt" ((s (option bool)) v_1)
              | Ensign -> unit_variant ctx "rank" 3 "Ensign"
      let _ = serialize_rank
      open! Serde
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_rank =
        let ( let* ) = Result.bind in
        let open Serde.De in
          fun ctx ->
            let field_visitor =
              Visitor.make
                ~visit_string:(fun _ctx ->
                                 fun str ->
                                   match str with
                                   | "Captain" -> Ok `Captain
                                   | "Commander" -> Ok `Commander
                                   | "Lt" -> Ok `Lt
                                   | "Ensign" -> Ok `Ensign
                                   | _ -> Error `invalid_tag) () in
            (variant ctx "rank" ["Captain"; "Commander"; "Lt"; "Ensign"]) @@
              (fun ctx ->
                 let* tag = identifier ctx field_visitor
                  in
                 match tag with
                 | `Captain ->
                     record_variant ctx 2
                       (fun ~size ->
                          fun ctx ->
                            ignore size;
                            (let field_visitor =
                               let visit_string _ctx str =
                                 match str with
                                 | "ship" -> Ok `ship
                                 | "name" -> Ok `name
                                 | _ -> Ok `invalid_tag in
                               let visit_int _ctx str =
                                 match str with
                                 | 0 -> Ok `ship
                                 | 1 -> Ok `name
                                 | _ -> Ok `invalid_tag in
                               Visitor.make ~visit_string ~visit_int () in
                             let name = ref None in
                             let ship = ref None in
                             let rec read_fields () =
                               let* tag = next_field ctx field_visitor
                                in
                               match tag with
                               | Some `ship ->
                                   let* v = field ctx "ship" string
                                    in (ship := (Some v); read_fields ())
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
                               Option.to_result
                                 ~none:(`Msg
                                          "missing field \"name\" (\"name\")")
                                 (!name)
                              in
                             let* ship =
                               Option.to_result
                                 ~none:(`Msg
                                          "missing field \"ship\" (\"ship\")")
                                 (!ship)
                              in Ok (Captain { ship; name })))
                 | `Commander ->
                     tuple_variant ctx 3
                       (fun ~size ->
                          fun ctx ->
                            ignore size;
                            (let* v_1 =
                               match element ctx string with
                               | Ok (Some v) -> Ok v
                               | Ok (None) -> Error `no_more_data
                               | Error reason -> Error reason
                              in
                             let* v_2 =
                               match element ctx int32 with
                               | Ok (Some v) -> Ok v
                               | Ok (None) -> Error `no_more_data
                               | Error reason -> Error reason
                              in
                             let* v_3 =
                               match element ctx float with
                               | Ok (Some v) -> Ok v
                               | Ok (None) -> Error `no_more_data
                               | Error reason -> Error reason
                              in Ok (Commander (v_1, v_2, v_3))))
                 | `Lt ->
                     (newtype_variant ctx) @@
                       ((fun ctx ->
                           let* v_1 = (d (option bool)) ctx
                            in Ok (Lt v_1)))
                 | `Ensign -> let* () = unit_variant ctx
                               in Ok Ensign)
      let _ = deserialize_rank
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type ranks =
    | Ranks of rank list [@@deriving (serialize, deserialize)]
  include
    struct
      let _ = fun (_ : ranks) -> ()
      let ( let* ) = Result.bind
      let _ = ( let* )
      let serialize_ranks =
        let open Serde.Ser in
          fun t ->
            fun ctx ->
              match t with
              | Ranks v_1 ->
                  newtype_variant ctx "ranks" 0 "Ranks"
                    ((s (list (s serialize_rank))) v_1)
      let _ = serialize_ranks
      open! Serde
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_ranks =
        let ( let* ) = Result.bind in
        let open Serde.De in
          fun ctx ->
            let field_visitor =
              Visitor.make
                ~visit_string:(fun _ctx ->
                                 fun str ->
                                   match str with
                                   | "Ranks" -> Ok `Ranks
                                   | _ -> Error `invalid_tag) () in
            (variant ctx "ranks" ["Ranks"]) @@
              (fun ctx ->
                 let* tag = identifier ctx field_visitor
                  in
                 match tag with
                 | `Ranks ->
                     (newtype_variant ctx) @@
                       ((fun ctx ->
                           let* v_1 = (d (list (d deserialize_rank))) ctx
                            in Ok (Ranks v_1))))
      let _ = deserialize_ranks
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () =
    let test_t =
      Ranks
        [Ensign;
        Commander ("riker", 2112l, Float.pi);
        Lt None;
        Lt (Some false);
        Lt (Some true);
        Captain { name = "janeway"; ship = "voyager" }] in
    let json1 = (Serde_json.to_string serialize_ranks test_t) |> Result.get_ok in
    let value = (Serde_json.of_string deserialize_ranks json1) |> Result.get_ok in
    let json2 = (Serde_json.to_string serialize_ranks value) |> Result.get_ok in
    Format.printf "[%s,%s]\n%!" json1 json2
