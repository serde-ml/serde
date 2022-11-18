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
  cookies = [ ("inline_tests", "enabled"); ("library-name", "test_de") ];
}]

let () = Ppx_inline_test_lib.Runtime.set_lib_and_partition "test_de" ""

open Serde

let ( let* ) = Result.bind

let parse eq fn s t =
  let t' = Serde_sexpr.of_string fn s |> Result.get_ok in
  eq t t'

let () = Ppx_inline_test_lib.Runtime.unset_lib "test_de"
