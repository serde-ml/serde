  $ dune clean
  $ dune build
  File "ppx_test.ml", lines 1-5, characters 0-37:
  1 | type rank = { 
  2 |   rank_name : string; 
  3 |   rank_scores : string list;
  4 | }
  5 | [@@deriving serializer, deserializer]
  Error: This expression has type
           ('a -> ('b, 'c, unit) ctx -> (unit, Serde.error) result) ->
           'a list -> ('b, 'c, unit) ctx -> (unit, Serde.error) result
         but an expression was expected of type
           ('a -> ('b, 'c, unit) ctx -> (unit, Serde.error) result, 'd, 'e) t =
             ('a -> ('b, 'c, unit) ctx -> (unit, Serde.error) result) ->
             ('a -> ('b, 'c, unit) ctx -> (unit, Serde.error) result, 'd, 'e)
             ctx -> ('e, Serde.error) result
         Type 'a list is not compatible with type
           ('a -> ('b, 'c, unit) ctx -> (unit, Serde.error) result, 'd, 'e) ctx
  [1]
  $ dune exec ./ppx_test.exe
  File "ppx_test.ml", lines 1-5, characters 0-37:
  1 | type rank = { 
  2 |   rank_name : string; 
  3 |   rank_scores : string list;
  4 | }
  5 | [@@deriving serializer, deserializer]
  Error: This expression has type
           ('a -> ('b, 'c, unit) ctx -> (unit, Serde.error) result) ->
           'a list -> ('b, 'c, unit) ctx -> (unit, Serde.error) result
         but an expression was expected of type
           ('a -> ('b, 'c, unit) ctx -> (unit, Serde.error) result, 'd, 'e) t =
             ('a -> ('b, 'c, unit) ctx -> (unit, Serde.error) result) ->
             ('a -> ('b, 'c, unit) ctx -> (unit, Serde.error) result, 'd, 'e)
             ctx -> ('e, Serde.error) result
         Type 'a list is not compatible with type
           ('a -> ('b, 'c, unit) ctx -> (unit, Serde.error) result, 'd, 'e) ctx
  [1]
  $ dune describe pp ./ppx_test.ml
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
    rank_name: string ;
    rank_scores: string list }[@@deriving (serializer, deserializer)]
  include
    struct
      let _ = fun (_ : rank) -> ()
      let serialize_rank =
        let ( let* ) = Result.bind in
        let open Serde.Ser in
          fun t ->
            fun ctx ->
              record ctx "rank" 2
                (fun ctx ->
                   let* () = field ctx "rank_name" (string t.rank_name)
                    in
                   let* () =
                     field ctx "rank_scores" ((s list string) t.rank_scores)
                    in Ok ())
      let _ = serialize_rank
      let deserialize_rank =
        let ( let* ) = Result.bind in
        let open Serde.De in
          fun t ->
            fun ctx ->
              record ctx "rank" 2
                (fun ctx ->
                   let* rank_name = field ctx "rank_name" string
                    in
                   let* rank_scores = field ctx "rank_scores" (d list string)
                    in Ok ())
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
      let serialize_t =
        let ( let* ) = Result.bind in
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
                   let* () = field ctx "credits" ((s option int32) t.credits)
                    in
                   let* () = field ctx "keywords" ((s array string) t.keywords)
                    in
                   let* () = field ctx "rank" ((s serialize_rank) t.rank)
                    in Ok ())
      let _ = serialize_t
      let deserialize_t =
        let ( let* ) = Result.bind in
        let open Serde.De in
          fun t ->
            fun ctx ->
              record ctx "t" 6
                (fun ctx ->
                   let* name = field ctx "name" string
                    in
                   let* commisioned = field ctx "commisioned" bool
                    in
                   let* updated_at = field ctx "updated_at" int64
                    in
                   let* credits = field ctx "credits" (d option int32)
                    in
                   let* keywords = field ctx "keywords" (d array string)
                    in
                   let* rank = field ctx "rank" (d deserialize_rank)
                    in Ok ())
      let _ = deserialize_t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  let () =
    let test_t =
      {
        name = "hello";
        commisioned = false;
        updated_at = (let open Int64 in sub max_int 1L);
        credits = None;
        keywords = [||];
        rank = { rank_name = "asdf"; rank_scores = ["1"; "c"; "a"] }
      } in
    let json = Serde_json.to_string serialize_t test_t in
    Format.printf "%s\n%!" json
