$ externally tagged variant

  $ dune clean
  $ dune exec ./variant_externally_tagged_test.exe | jq .
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
  $ dune describe pp ./variant_externally_tagged_test.ml
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
        let _ = ( let* ) in
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
        let _ = ( let* ) in
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

$ adjacently tagged variant

  $ dune exec ./variant_adjacently_tagged_test.exe | jq .
  [
    {
      "Ranks": [
        {
          "t": "Ensign"
        },
        {
          "t": "Commander",
          "c": [
            "riker",
            2112,
            3.14159265359
          ]
        },
        {
          "t": "Lt",
          "c": null
        },
        {
          "t": "Lt",
          "c": false
        },
        {
          "t": "Lt",
          "c": true
        },
        {
          "t": "Captain",
          "c": {
            "name": "janeway",
            "ship": "voyager"
          }
        }
      ]
    },
    {
      "Ranks": [
        {
          "t": "Ensign"
        },
        {
          "t": "Commander",
          "c": [
            "riker",
            2112,
            3.14159265359
          ]
        },
        {
          "t": "Lt",
          "c": null
        },
        {
          "t": "Lt",
          "c": false
        },
        {
          "t": "Lt",
          "c": true
        },
        {
          "t": "Captain",
          "c": {
            "name": "janeway",
            "ship": "voyager"
          }
        }
      ]
    }
  ]
  $ dune describe pp ./variant_adjacently_tagged_test.ml
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
    | Ensign [@@deriving (serialize, deserialize)][@@serde
                                                    { tag = "t"; content = "c"
                                                    }]
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
                  record ctx "" 2
                    (fun ctx ->
                       let* () = field ctx "t" (string "Captain")
                        in
                       field ctx "c"
                         (fun ctx ->
                            record ctx "rank" 2
                              (fun ctx ->
                                 let* () = field ctx "name" (string r.name)
                                  in
                                 let* () = field ctx "ship" (string r.ship)
                                  in Ok ())))
              | Commander (v_1, v_2, v_3) ->
                  record ctx "" 2
                    (fun ctx ->
                       let* () = field ctx "t" (string "Commander")
                        in
                       field ctx "c"
                         (fun ctx ->
                            sequence ctx 3
                              (fun ctx ->
                                 let* () = element ctx (string v_1)
                                  in
                                 let* () = element ctx (int32 v_2)
                                  in let* () = element ctx (float v_3)
                                      in Ok ())))
              | Lt v_1 ->
                  record ctx "" 2
                    (fun ctx ->
                       let* () = field ctx "t" (string "Lt")
                        in field ctx "c" (fun ctx -> (s (option bool)) v_1 ctx))
              | Ensign ->
                  record ctx "" 1 (fun ctx -> field ctx "t" (string "Ensign"))
      let _ = serialize_rank
      open! Serde
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_rank =
        let ( let* ) = Result.bind in
        let _ = ( let* ) in
        let open Serde.De in
          fun ctx ->
            let tag_content_field_visitor =
              Visitor.make
                ~visit_string:(fun _ctx ->
                                 fun str ->
                                   match str with
                                   | "t" -> Ok `tag
                                   | "c" -> Ok `content
                                   | _ -> Error `invalid_tag) () in
            record ctx "" 2
              (fun ctx ->
                 let rec read_fields ctx =
                   let* field_name = next_field ctx tag_content_field_visitor
                    in
                   match field_name with
                   | Some `tag ->
                       let rec inner_read_fields () =
                         let* variant = field ctx "t" string
                          in
                         let* field_name =
                           next_field ctx tag_content_field_visitor
                          in
                         match field_name with
                         | Some `content ->
                             (match variant with
                              | "Captain" ->
                                  record ctx "" 2
                                    (fun ctx ->
                                       let field_visitor =
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
                                         Visitor.make ~visit_string ~visit_int
                                           () in
                                       let name = ref None in
                                       let ship = ref None in
                                       let rec read_fields () =
                                         let* tag =
                                           next_field ctx field_visitor
                                          in
                                         match tag with
                                         | Some `ship ->
                                             let* v = field ctx "ship" string
                                              in
                                             (ship := (Some v); read_fields ())
                                         | Some `name ->
                                             let* v = field ctx "name" string
                                              in
                                             (name := (Some v); read_fields ())
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
                                        in Ok (Captain { ship; name }))
                              | "Commander" ->
                                  sequence ctx
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
                              | "Lt" ->
                                  (deserialize ctx) @@
                                    ((fun ctx ->
                                        let* v_1 = (d (option bool)) ctx
                                         in Ok (Lt v_1)))
                              | "Ensign" -> Ok Ensign
                              | _ ->
                                  Error
                                    (`Msg "variant constructor not recognized"))
                         | Some `tag -> Error (`Msg "duplicate field \"t\"")
                         | Some `invalid_tag ->
                             let* () = ignore_any ctx
                              in inner_read_fields ()
                         | None ->
                             (match variant with
                              | "Captain" ->
                                  record ctx "" 2
                                    (fun ctx ->
                                       let field_visitor =
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
                                         Visitor.make ~visit_string ~visit_int
                                           () in
                                       let name = ref None in
                                       let ship = ref None in
                                       let rec read_fields () =
                                         let* tag =
                                           next_field ctx field_visitor
                                          in
                                         match tag with
                                         | Some `ship ->
                                             let* v = field ctx "ship" string
                                              in
                                             (ship := (Some v); read_fields ())
                                         | Some `name ->
                                             let* v = field ctx "name" string
                                              in
                                             (name := (Some v); read_fields ())
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
                                        in Ok (Captain { ship; name }))
                              | "Commander" ->
                                  sequence ctx
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
                              | "Lt" ->
                                  (deserialize ctx) @@
                                    ((fun ctx ->
                                        let* v_1 = (d (option bool)) ctx
                                         in Ok (Lt v_1)))
                              | "Ensign" -> Ok Ensign
                              | _ ->
                                  Error
                                    (`Msg "variant constructor not recognized")) in
                       inner_read_fields ()
                   | Some `content ->
                       Error
                         (`Msg
                            "field \"t\" must appear first, found \"c\" instead")
                   | Some `invalid_tag ->
                       let* () = ignore_any ctx
                        in read_fields ctx
                   | None -> Error (`Msg "missing field \"t\"") in
                 read_fields ctx)
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
        let _ = ( let* ) in
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


$ internally tagged variant

  $ dune exec ./variant_internally_tagged_test.exe | jq .
  File "dune", line 16, characters 14-32:
  16 |   (preprocess (pps serde_derive))
                     ^^^^^^^^^^^^^^^^^^
  Fatal error: exception Failure("not implemented")
  $ dune describe pp ./variant_internally_tagged_test.ml
  File "dune", line 16, characters 14-32:
  16 |   (preprocess (pps serde_derive))
                     ^^^^^^^^^^^^^^^^^^
  Fatal error: exception Failure("not implemented")
  [1]
