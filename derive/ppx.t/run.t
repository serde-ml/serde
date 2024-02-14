  $ dune clean
  $ dune exec ./record_test.exe | jq .
  [
    {
      "stuff": [
        {
          "name": "hello",
          "commisioned": false,
          "updated_at": 9223372036854775806,
          "credits": null,
          "keywords": [],
          "rank": {
            "rank_scores": [
              "1",
              "c",
              "a"
            ],
            "rank_name": "asdf"
          }
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
          }
        }
      ]
    },
    {
      "stuff": [
        {
          "name": "hello",
          "commisioned": false,
          "updated_at": 9223372036854775806,
          "credits": null,
          "keywords": [],
          "rank": {
            "rank_scores": [
              "1",
              "c",
              "a"
            ],
            "rank_name": "asdf"
          }
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
          }
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
    rank_name: string }[@@deriving (serializer, deserializer)]
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
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_rank =
        let ( let* ) = Result.bind in
        let open Serde.De in
          fun ctx ->
            record ctx "rank" 2
              (fun ctx ->
                 let* rank_scores = field ctx "rank_scores" (d (list string))
                  in
                 let* rank_name = field ctx "rank_name" string
                  in Ok { rank_scores; rank_name })
      let _ = deserialize_rank
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type t =
    {
    name: string ;
    commisioned: bool ;
    updated_at: int64 ;
    credits: int32 option ;
    keywords: string array ;
    rank: rank }[@@deriving (serializer, deserializer)]
  include
    struct
      let _ = fun (_ : t) -> ()
      let ( let* ) = Result.bind
      let _ = ( let* )
      let serialize_t =
        let open Serde.Ser in
          fun t ->
            fun ctx ->
              record ctx "t" 6
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
                    in Ok ())
      let _ = serialize_t
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_t =
        let ( let* ) = Result.bind in
        let open Serde.De in
          fun ctx ->
            record ctx "t" 6
              (fun ctx ->
                 let* name = field ctx "name" string
                  in
                 let* commisioned = field ctx "commisioned" bool
                  in
                 let* updated_at = field ctx "updated_at" int64
                  in
                 let* credits = field ctx "credits" (d (option int32))
                  in
                 let* keywords = field ctx "keywords" (d (array string))
                  in
                 let* rank = field ctx "rank" (d deserialize_rank)
                  in
                 Ok { name; commisioned; updated_at; credits; keywords; rank })
      let _ = deserialize_t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type t_list = {
    stuff: t list }[@@deriving (serializer, deserializer)]
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
      let ( let* ) = Result.bind
      let _ = ( let* )
      let deserialize_t_list =
        let ( let* ) = Result.bind in
        let open Serde.De in
          fun ctx ->
            record ctx "t_list" 1
              (fun ctx ->
                 let* stuff = field ctx "stuff" (d (list (d deserialize_t)))
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
             updated_at = ((let open Int64 in sub max_int 1L));
             credits = None;
             keywords = [||];
             rank = { rank_name = "asdf"; rank_scores = ["1"; "c"; "a"] }
           };
          {
            name = "hello";
            commisioned = false;
            updated_at = 0L;
            credits = (Some 2112l);
            keywords = [|"hello"|];
            rank = { rank_name = "asdf"; rank_scores = [] }
          }]
      } in
    let json1 = (Serde_json.to_string serialize_t_list test_t) |> Result.get_ok in
    let value =
      (Serde_json.of_string deserialize_t_list json1) |> Result.get_ok in
    let json2 = (Serde_json.to_string serialize_t_list value) |> Result.get_ok in
    Format.printf "[%s,%s]\n%!" json1 json2




Now we test the variants:

  $ dune exec ./variant_test.exe | jq .
  "Ensign"
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
    | Captain 
    | Commander 
    | Lt 
    | Ensign [@@deriving serializer]
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
              | Captain -> unit_variant ctx "rank" 0 "Captain"
              | Commander -> unit_variant ctx "rank" 1 "Commander"
              | Lt -> unit_variant ctx "rank" 2 "Lt"
              | Ensign -> unit_variant ctx "rank" 3 "Ensign"
      let _ = serialize_rank
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () =
    let test_t = Ensign in
    let json1 = (Serde_json.to_string serialize_rank test_t) |> Result.get_ok in
    Format.printf "%s\n%!" json1
