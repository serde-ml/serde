let ( let* ) = Result.bind

let pp_list pp_el fmt t =
  Format.fprintf fmt "[";
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
    pp_el fmt t;
  Format.fprintf fmt "]"

type error =
  [ `invalid_field_type
  | `missing_field
  | `no_more_data
  | `unimplemented
  | `invalid_tag
  | `io_error of Rio.io_error ]

let pp_err fmt t =
  match t with
  | `invalid_field_type -> Format.fprintf fmt "invalid_field_type"
  | `missing_field -> Format.fprintf fmt "missing_field"
  | `no_more_data -> Format.fprintf fmt "no_more_data"
  | `unimplemented -> Format.fprintf fmt "unimplemented"
  | `invalid_tag -> Format.fprintf fmt "invalid_tag"
  | `io_error err -> Rio.pp_err fmt err

module Config = struct
  type t = { camelcase_fields : bool }

  let default = { camelcase_fields = false }
end

module rec Ser : sig
  type ('value, 'state, 'output) ser_fn =
    ('value, 'state, 'output) ctx -> 'value -> ('output, error) result

  and ('value, 'state, 'output) t =
    | Serialize of ('value, 'state, 'output) ser_fn

  and ('value, 'state, 'output) ctx =
    ('value, 'state, 'output) t * ('state, 'output) Ser.serializer * 'state

  val serializer :
    ('value, 'state, 'output) ser_fn -> ('value, 'state, 'output) t

  module type Serializer = sig
    type output
    type state

    val serialize_variant :
      ('value, state, output) Ser.t ->
      state ->
      var_type:string ->
      cstr_idx:int ->
      cstr_name:string ->
      cstr_args:int ->
      (output, error) result
  end

  type ('state, 'output) serializer =
    (module Serializer with type output = 'output and type state = 'state)

  val variant :
    ('value, 'state, 'output) ctx ->
    string ->
    int ->
    string ->
    int ->
    ('output, error) result
end = struct
  type ('value, 'state, 'output) ser_fn =
    ('value, 'state, 'output) ctx -> 'value -> ('output, error) result

  and ('value, 'state, 'output) t =
    | Serialize of ('value, 'state, 'output) ser_fn

  and ('value, 'state, 'output) ctx =
    ('value, 'state, 'output) t * ('state, 'output) Ser.serializer * 'state

  let serializer fn = Serialize fn

  module type Serializer = sig
    type output
    type state

    val serialize_variant :
      ('value, state, output) Ser.t ->
      state ->
      var_type:string ->
      cstr_idx:int ->
      cstr_name:string ->
      cstr_args:int ->
      (output, error) result
  end

  type ('state, 'output) serializer =
    (module Serializer with type output = 'output and type state = 'state)

  let variant (type value state output)
      ((self, (module S), state) : (value, state, output) ctx) var_type cstr_idx
      cstr_name cstr_args =
    S.serialize_variant self state ~var_type ~cstr_idx ~cstr_name ~cstr_args
end

module rec De_base : sig
  type ('value, 'state) t =
    | Deserialize of ('state De_base.ctx -> ('value, error) result)

  and 'state ctx = 'state De_base.deserializer * 'state

  type ('value, 'state, 'tag) visitor = {
    visit_string : 'state ctx -> string -> ('value, error) result;
    visit_variant : 'state ctx -> ('value, error) result;
  }

  val deserializer :
    ('state De_base.ctx -> ('value, error) result) -> ('value, 'state) t

  module type Deserializer = sig
    type state

    val deserialize_variant :
      state ctx ->
      state ->
      ('value, state, 'tag) visitor ->
      name:string ->
      variants:string list ->
      ('value, error) result

    val deserialize_identifier :
      state ctx ->
      state ->
      ('value, state, 'tag) visitor ->
      ('value, error) result

    val deserialize_string :
      state ctx ->
      state ->
      ('value, state, 'tag) visitor ->
      ('value, error) result
  end

  type 'state deserializer = (module Deserializer with type state = 'state)
end = struct
  type ('value, 'state) t =
    | Deserialize of ('state De_base.ctx -> ('value, error) result)

  and 'state ctx = 'state De_base.deserializer * 'state

  type ('value, 'state, 'tag) visitor = {
    visit_string : 'state ctx -> string -> ('value, error) result;
    visit_variant : 'state ctx -> ('value, error) result;
  }

  let deserializer fn = Deserialize fn

  module type Deserializer = sig
    type state

    val deserialize_variant :
      state ctx ->
      state ->
      ('value, state, 'tag) visitor ->
      name:string ->
      variants:string list ->
      ('value, error) result

    val deserialize_identifier :
      state ctx ->
      state ->
      ('value, state, 'tag) visitor ->
      ('value, error) result

    val deserialize_string :
      state ctx ->
      state ->
      ('value, state, 'tag) visitor ->
      ('value, error) result
  end

  type 'state deserializer = (module Deserializer with type state = 'state)
end

module Visitor = struct
  type ('value, 'state, 'tag) t = ('value, 'state, 'tag) De_base.visitor = {
    visit_string : 'state De_base.ctx -> string -> ('value, error) result;
    visit_variant : 'state De_base.ctx -> ('value, error) result;
  }

  let default =
    De_base.
      {
        visit_string = (fun _ctx _str -> Error `unimplemented);
        visit_variant = (fun _ctx -> Error `unimplemented);
      }

  let visit_variant ctx t = t.visit_variant ctx
  let visit_string ctx t str = t.visit_string ctx str
end

module De = struct
  include De_base

  let deserialize_variant (type state) (((module D), state) as ctx : state ctx)
      ~visitor ~name ~variants =
    D.deserialize_variant ctx state visitor ~name ~variants

  let deserialize_identifier (type state)
      (((module D), state) as ctx : state ctx) visitor =
    D.deserialize_identifier ctx state visitor

  let deserialize_string (type state) (((module D), state) as ctx : state ctx)
      visitor =
    D.deserialize_string ctx state visitor

  let variant ctx name variants visit_variant =
    let visitor = { Visitor.default with visit_variant } in
    deserialize_variant ctx ~visitor ~name ~variants

  let identifier ctx visitor = deserialize_identifier ctx visitor
end

module Serializer = struct
  type ('state, 'output) t = ('state, 'output) Ser.serializer

  module type Intf = Ser.Serializer

  module Default = struct end
end

module Deserializer = struct
  type 'state t = 'state De.deserializer

  module type Intf = De.Deserializer

  module Default = struct end
end

let serialize :
    type value state output.
    (state, output) Serializer.t ->
    state ->
    (value, state, output) Ser.t ->
    value ->
    (output, error) result =
 fun fmt ctx (Serialize ser as self) value -> ser (self, fmt, ctx) value

let deserialize :
    type value state output.
    state Deserializer.t ->
    state ->
    (value, state) De.t ->
    (value, error) result =
 fun fmt ctx (Deserialize de) -> de (fmt, ctx)
