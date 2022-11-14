(*
open Ppxlib

let generate_impl ~ctxt (rec_flag, type_declarations) =
  ...

let generate_intf ~ctxt (rec_flag, type_declarations) =
  ...

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let my_deriver =
  Deriving.add
    "deserializer"
    ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
    *)
