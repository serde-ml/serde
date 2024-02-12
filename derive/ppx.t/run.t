  $ dune clean
  $ dune build
  File "ppx_test.ml", lines 1-5, characters 0-23:
  1 | type rank = { 
  2 |   rank_name : string; 
  3 |   rank_score : int32;
  4 | }
  5 | [@@deriving serializer]
  Error: Unbound value r
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
      cookies = [("library-name", "ppx_test")]
    }]
  type rank = {
    rank_name: string ;
    rank_score: int32 }[@@deriving serializer]
  include
    struct
      let _ = fun (_ : rank) -> ()
      let serialize_rank =
        let ( let* ) = Result.bind in
        let open Serde.Ser in
          fun t ->
            fun ctx ->
              (Serde.Ser.record ctx "rank" 2) @@
                (fun ctx ->
                   let* () = field ctx "rank_name" (string r.rank_name)
                    in
                   let* () = field ctx "rank_score" (int32 r.rank_score)
                    in Ok ())
      let _ = serialize_rank
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type t =
    {
    name: string ;
    commisioned: bool ;
    updated_at: int64 ;
    credits: int32 option ;
    keywords: string array ;
    rank: rank }[@@deriving serializer]
  include
    struct
      let _ = fun (_ : t) -> ()
      let serialize_t =
        let ( let* ) = Result.bind in
        let open Serde.Ser in
          fun t ->
            fun ctx ->
              (Serde.Ser.record ctx "t" 6) @@
                (fun ctx ->
                   let* () = field ctx "name" (string r.name)
                    in
                   let* () = field ctx "commisioned" (bool r.commisioned)
                    in
                   let* () = field ctx "updated_at" (int64 r.updated_at)
                    in
                   let* () =
                     field ctx "credits" ((s serialize_option) r.credits)
                    in
                   let* () = field ctx "keywords" ((array string) r.keywords)
                    in
                   let* () = field ctx "rank" ((s serialize_rank) r.rank)
                    in Ok ())
      let _ = serialize_t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
