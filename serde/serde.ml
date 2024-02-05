let ( let* ) = Result.bind

let pp_list pp_el fmt t =
  Format.fprintf fmt "[";
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
    pp_el fmt t;
  Format.fprintf fmt "]"

type error =
  [ `invalid_field_type | `missing_field | `no_more_data | `unimplemented ]

let pp_err fmt t =
  match t with
  | `invalid_field_type -> Format.fprintf fmt "invalid_field_type"
  | `missing_field -> Format.fprintf fmt "missing_field"
  | `no_more_data -> Format.fprintf fmt "no_more_data"
  | `unimplemented -> Format.fprintf fmt "unimplemented"

module Chain = struct
  type (_, _) chain =
    | Run : 'a -> ('input, 'a) chain
    | Chain :
        ('input, 'output -> 'a) chain * ('input -> ('output, error) result)
        -> ('input, 'a) chain

  let run fn = Run fn
  let chain fn run = Chain (run, fn)

  let rec apply :
      type v i o.
      between:(i -> (unit, error) result) ->
      (i, v -> o) chain ->
      v ->
      i ->
      (o, error) result =
   fun ~between chain v input ->
    match chain with
    | Run fn -> Ok (fn v)
    | Chain (chain, fn) ->
        let* v1 = fn input in
        let* () = between input in
        let* next = apply ~between chain v1 input in
        Ok (next v)

  let execute :
      type i o.
      between:(i -> (unit, error) result) ->
      (i, o) chain ->
      i ->
      (o, error) result =
   fun ~between chain input ->
    match chain with
    | Run _fn -> assert false
    | Chain (next, fn) ->
        let* v = fn input in
        let* () = between input in
        apply ~between next v input

  let execute ?(between = fun _ -> Ok ()) chain input =
    let* result = execute ~between chain input in
    result
end

module Config = struct
  type t = { camelcase_fields : bool }

  let default = { camelcase_fields = false }
end

module Ser = struct
  type t =
    | Bool of bool
    | Int of int
    | Str of string
    | Record of record
    | Variant_cstr of variant
    | Variant_record of variant_record

  and field = { fld_name : string; fld_value : t }
  and record = { rec_type : string; rec_fields : field list }

  and variant = {
    vcstr_type : string;
    vcstr_name : string;
    vcstr_args : t list;
  }

  and variant_record = {
    vrec_type : string;
    vrec_name : string;
    vrec_fields : field list;
  }

  let rec pp fmt t =
    match t with
    | Bool b -> Format.fprintf fmt "(Bool %b)" b
    | Int i -> Format.fprintf fmt "(Int %d)" i
    | Record { rec_type; rec_fields } ->
        Format.fprintf fmt "(Record (%S, " rec_type;
        pp_list pp_field fmt rec_fields;
        Format.fprintf fmt "))"
    | Str s -> Format.fprintf fmt "(Str %S)" s
    | Variant_cstr { vcstr_type; vcstr_name; vcstr_args } ->
        Format.fprintf fmt "(Variant {vcstr_type=%S;vcstr_name=%S; vcstr_args="
          vcstr_type vcstr_name;
        pp_list pp fmt vcstr_args;
        Format.fprintf fmt "})"
    | Variant_record { vrec_type; vrec_name; vrec_fields } ->
        Format.fprintf fmt
          "(Variant_record {vrec_type=%S;vrec_name=%S; vrec_fields=" vrec_type
          vrec_name;
        pp_list pp_field fmt vrec_fields;
        Format.fprintf fmt "})"

  and pp_field fmt { fld_name; fld_value } =
    Format.fprintf fmt "{ fld_name=%S; fld_value=%a }" fld_name pp fld_value

  let record rec_type rec_fields = Record { rec_type; rec_fields }

  let variant vcstr_type (vcstr_name, vcstr_args) =
    Variant_cstr { vcstr_type; vcstr_name; vcstr_args }

  let variant_record vrec_type (vrec_name, vrec_fields) =
    Variant_record { vrec_type; vrec_name; vrec_fields }

  let field fld_name fld_value = { fld_name; fld_value }
  let constructor name args = (name, args)
  let int i = Int i
  let string s = Str s
  let bool b = Bool b
end

module Serializer = struct
  module type Intf = sig
    type output

    val serialize_bool : Config.t -> bool -> (output, error) result
    val serialize_int : Config.t -> int -> (output, error) result
    val serialize_string : Config.t -> string -> (output, error) result

    val serialize_record :
      (Ser.t -> (output, error) result) ->
      Config.t ->
      Ser.record ->
      (output, error) result

    val serialize_variant :
      (Ser.t -> (output, error) result) ->
      Config.t ->
      Ser.variant ->
      (output, error) result

    val serialize_variant_record :
      (Ser.t -> (output, error) result) ->
      Config.t ->
      Ser.variant_record ->
      (output, error) result
  end

  module Default = struct
    let serialize_bool _ _ = Error `unimplemented
    let serialize_int _ _ = Error `unimplemented
    let serialize_string _ _ = Error `unimplemented
    let serialize_record _ _ = Error `unimplemented
    let serialize_variant _ _ = Error `unimplemented
    let serialize_variant_record _ _ = Error `unimplemented
  end

  let rec map_fields serializer xs =
    match xs with
    | [] -> Ok []
    | Ser.{ fld_name; fld_value } :: xs ->
        let* value = serializer fld_value in
        let* tail = map_fields serializer xs in
        Ok ((fld_name, value) :: tail)

  let rec map serializer xs =
    match xs with
    | [] -> Ok []
    | x :: xs ->
        let* value = serializer x in
        let* tail = map serializer xs in
        Ok (value :: tail)
end

type 'fmt serializer = (module Serializer.Intf with type output = 'fmt)

let rec serialize :
    type fmt value.
    ?config:Config.t ->
    fmt serializer ->
    (value -> Ser.t) ->
    value ->
    (fmt, error) result =
 fun ?(config = Config.default) fmt shape value ->
  let self data = serialize ~config fmt (fun x -> x) data in
  let (module Fmt) = fmt in
  let data = shape value in
  match data with
  | Ser.Bool b -> Fmt.serialize_bool config b
  | Ser.Int i -> Fmt.serialize_int config i
  | Ser.Record r -> Fmt.serialize_record self config r
  | Ser.Str s -> Fmt.serialize_string config s
  | Ser.Variant_cstr v -> Fmt.serialize_variant self config v
  | Ser.Variant_record r -> Fmt.serialize_variant_record self config r

module rec De_base : sig
  type 'input build_ctx =
    Config.t * (module De_base.Intf with type input = 'input) * 'input

  type ('input, 'value) builder = ('input build_ctx, 'value) Chain.chain

  type ('input, 'value) t = Variant of ('input, 'value) variant

  and ('input, 'value) variant = {
    var_name : string;
    var_cstrs : ('input, 'value) cstr list;
  }

  and ('input, 'value) cstr =
    | Cstr_unit of { ucstr_name : string; ucstr_val : 'value }
    | Cstr_args of { cstr_name : string; cstr_fn : ('input, 'value) builder }

  module type Intf = sig
    type input

    val deserialize_int : Config.t -> input -> (int, error) result
    val deserialize_string : Config.t -> input -> (string, error) result

    val deserialize_variant :
      Config.t ->
      (module De_base.Intf with type input = input) ->
      (input, ('value, error) result) variant ->
      input ->
      ('value, error) result
  end
end = struct
  type 'input build_ctx =
    Config.t * (module De_base.Intf with type input = 'input) * 'input

  type ('input, 'value) builder = ('input build_ctx, 'value) Chain.chain

  type ('input, 'value) t = Variant of ('input, 'value) variant

  and ('input, 'value) variant = {
    var_name : string;
    var_cstrs : ('input, 'value) cstr list;
  }

  and ('input, 'value) cstr =
    | Cstr_unit of { ucstr_name : string; ucstr_val : 'value }
    | Cstr_args of { cstr_name : string; cstr_fn : ('input, 'value) builder }

  module type Intf = sig
    type input

    val deserialize_int : Config.t -> input -> (int, error) result
    val deserialize_string : Config.t -> input -> (string, error) result

    val deserialize_variant :
      Config.t ->
      (module De_base.Intf with type input = input) ->
      (input, ('value, error) result) variant ->
      input ->
      ('value, error) result
  end
end

module De = struct
  include De_base

  let rec pp fmt t =
    match t with
    | Variant { var_name; var_cstrs } ->
        Format.fprintf fmt "(Variant {var_name=%S;var_cstrs=" var_name;
        (pp_list pp_cstr) fmt var_cstrs;
        Format.fprintf fmt "})"

  and pp_cstr fmt cstr =
    match cstr with
    | Cstr_unit { ucstr_name; _ } ->
        Format.fprintf fmt "(Cstr_unit {ucstr_name=%S; ucstr_val=_})" ucstr_name
    | Cstr_args { cstr_name; _ } ->
        Format.fprintf fmt
          "(Cstr_args {cstr_name=%S; cstr_fn=(fun _ser -> Error \
           `unimplemented)})"
          cstr_name

  let variant var_name var_cstrs = Variant { var_name; var_cstrs }

  let unit_constructor ucstr_name ucstr_val =
    Cstr_unit { ucstr_name; ucstr_val }

  let constructor cstr_name cstr_fn =
    Cstr_args { cstr_name; cstr_fn = Chain.run cstr_fn }

  let arg arg cstr =
    match cstr with
    | Cstr_unit _ -> failwith "Cannot add arguments to unit constructor"
    | Cstr_args { cstr_name; cstr_fn } ->
        Cstr_args { cstr_name; cstr_fn = Chain.chain arg cstr_fn }

  let int (type input)
      (config, (module De : Intf with type input = input), (input : input)) =
    let* int = De.deserialize_int config input in
    Ok int

  let string (type input)
      (config, (module De : Intf with type input = input), (input : input)) =
    let* string = De.deserialize_string config input in
    Ok string
end

module Deserializer = struct
  module type Intf = De_base.Intf
end

type 'src deserializer = (module Deserializer.Intf with type input = 'src)

let deserialize :
    type src value.
    ?config:Config.t ->
    src deserializer ->
    (src, (value, error) result) De.t ->
    src ->
    (value, error) result =
 fun ?(config = Config.default) fmt shape input ->
  let (module Fmt) = fmt in
  match shape with De.Variant v -> Fmt.deserialize_variant config fmt v input
