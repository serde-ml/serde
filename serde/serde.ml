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
  | `Msg of string
  | Rio.io_error ]

let pp_err fmt t =
  match t with
  | `invalid_field_type -> Format.fprintf fmt "invalid_field_type"
  | `missing_field -> Format.fprintf fmt "missing_field"
  | `no_more_data -> Format.fprintf fmt "no_more_data"
  | `unimplemented -> Format.fprintf fmt "unimplemented"
  | `invalid_tag -> Format.fprintf fmt "invalid_tag"
  | `Msg str -> Format.fprintf fmt "%S" str
  | #Rio.io_error as err -> Rio.pp_err fmt err

module Config = struct
  type t = { camelcase_fields : bool }

  let default = { camelcase_fields = false }
end

module rec Ser_base : sig
  type ('value, 'state, 'output) t =
    'value -> ('value, 'state, 'output) ctx -> ('output, error) result

  and ('value, 'state, 'output) ctx =
    | Ctx of
        ('value, 'state, 'output) t
        * ('state, 'output) Ser_base.serializer
        * 'state

  val serializer : ('value, 'state, 'output) t -> ('value, 'state, 'output) t

  module type Serializer = sig
    type output
    type state

    val nest : state -> state

    val serialize_bool :
      ('value, state, output) ctx -> state -> bool -> (output, error) result

    val serialize_int8 :
      ('value, state, output) ctx -> state -> char -> (output, error) result

    val serialize_int16 :
      ('value, state, output) ctx -> state -> int -> (output, error) result

    val serialize_int31 :
      ('value, state, output) ctx -> state -> int -> (output, error) result

    val serialize_int32 :
      ('value, state, output) ctx -> state -> int32 -> (output, error) result

    val serialize_int64 :
      ('value, state, output) ctx -> state -> int64 -> (output, error) result

    val serialize_float :
      ('value, state, output) ctx -> state -> float -> (output, error) result

    val serialize_string :
      ('value, state, output) ctx -> state -> string -> (output, error) result

    val serialize_none :
      ('value, state, output) ctx -> state -> (output, error) result

    val serialize_some :
      ('value, state, output) ctx ->
      state ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_sequence :
      ('value, state, output) ctx ->
      state ->
      size:int ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_element :
      ('value, state, output) ctx ->
      state ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_unit_variant :
      ('value, state, output) ctx ->
      state ->
      var_type:string ->
      cstr_idx:int ->
      cstr_name:string ->
      (output, error) result

    val serialize_newtype_variant :
      ('value, state, output) ctx ->
      state ->
      var_type:string ->
      cstr_idx:int ->
      cstr_name:string ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_tuple_variant :
      ('value, state, output) ctx ->
      state ->
      var_type:string ->
      cstr_idx:int ->
      cstr_name:string ->
      size:int ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_record_variant :
      ('value, state, output) ctx ->
      state ->
      var_type:string ->
      cstr_idx:int ->
      cstr_name:string ->
      size:int ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_record :
      ('value, state, output) ctx ->
      state ->
      rec_type:string ->
      size:int ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_field :
      ('value, state, output) ctx ->
      state ->
      name:string ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result
  end

  type ('state, 'output) serializer =
    (module Serializer with type output = 'output and type state = 'state)
end = struct
  type ('value, 'state, 'output) t =
    'value -> ('value, 'state, 'output) ctx -> ('output, error) result

  and ('value, 'state, 'output) ctx =
    | Ctx of
        ('value, 'state, 'output) t
        * ('state, 'output) Ser_base.serializer
        * 'state

  let serializer fn = fn

  module type Serializer = sig
    type output
    type state

    val nest : state -> state

    val serialize_bool :
      ('value, state, output) ctx -> state -> bool -> (output, error) result

    val serialize_int8 :
      ('value, state, output) ctx -> state -> char -> (output, error) result

    val serialize_int16 :
      ('value, state, output) ctx -> state -> int -> (output, error) result

    val serialize_int31 :
      ('value, state, output) ctx -> state -> int -> (output, error) result

    val serialize_int32 :
      ('value, state, output) ctx -> state -> int32 -> (output, error) result

    val serialize_int64 :
      ('value, state, output) ctx -> state -> int64 -> (output, error) result

    val serialize_float :
      ('value, state, output) ctx -> state -> float -> (output, error) result

    val serialize_string :
      ('value, state, output) ctx -> state -> string -> (output, error) result

    val serialize_none :
      ('value, state, output) ctx -> state -> (output, error) result

    val serialize_some :
      ('value, state, output) ctx ->
      state ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_sequence :
      ('value, state, output) ctx ->
      state ->
      size:int ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_element :
      ('value, state, output) ctx ->
      state ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_unit_variant :
      ('value, state, output) ctx ->
      state ->
      var_type:string ->
      cstr_idx:int ->
      cstr_name:string ->
      (output, error) result

    val serialize_newtype_variant :
      ('value, state, output) ctx ->
      state ->
      var_type:string ->
      cstr_idx:int ->
      cstr_name:string ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_tuple_variant :
      ('value, state, output) ctx ->
      state ->
      var_type:string ->
      cstr_idx:int ->
      cstr_name:string ->
      size:int ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_record_variant :
      ('value, state, output) ctx ->
      state ->
      var_type:string ->
      cstr_idx:int ->
      cstr_name:string ->
      size:int ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_record :
      ('value, state, output) ctx ->
      state ->
      rec_type:string ->
      size:int ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result

    val serialize_field :
      ('value, state, output) ctx ->
      state ->
      name:string ->
      (('value, state, output) ctx -> (output, error) result) ->
      (output, error) result
  end

  type ('state, 'output) serializer =
    (module Serializer with type output = 'output and type state = 'state)
end

module Ser = struct
  include Ser_base

  let serialize (type value state output) (ctx : (value, state, output) ctx)
      (ser : (value, state, output) ctx -> (output, error) result) :
      (output, error) result =
    ser ctx

  let serialize_sequence (type value state output)
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) size
      elements =
    S.serialize_sequence self state ~size elements

  let serialize_record (type value state output)
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) rec_type
      size fields =
    S.serialize_record self state ~rec_type ~size fields

  let sequence (type value state output)
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) size
      elements =
    S.serialize_sequence self state ~size elements

  let unit_variant (type value state output)
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) var_type
      cstr_idx cstr_name =
    S.serialize_unit_variant self state ~var_type ~cstr_idx ~cstr_name

  let newtype_variant (type value state output)
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) var_type
      cstr_idx cstr_name value =
    S.serialize_newtype_variant self state ~var_type ~cstr_idx ~cstr_name value

  let tuple_variant (type value state output)
      (Ctx (_, (module S), state) as ctx : (value, state, output) ctx) var_type
      cstr_idx cstr_name size =
    S.serialize_tuple_variant ctx state ~var_type ~cstr_idx ~cstr_name ~size

  let record_variant (type value state output)
      (Ctx (_, (module S), state) as ctx : (value, state, output) ctx) var_type
      cstr_idx cstr_name size =
    S.serialize_record_variant ctx state ~var_type ~cstr_idx ~cstr_name ~size

  let record (type value state output)
      (Ctx (_, (module S), state) as ctx : (value, state, output) ctx) rec_type
      size =
    S.serialize_record ctx state ~rec_type ~size

  let element (type value state output)
      (Ctx (_, (module S), state) as ctx : (value, state, output) ctx) value =
    S.serialize_element ctx state value

  let field (type value state output)
      (Ctx (_, (module S), state) as ctx : (value, state, output) ctx) name
      value =
    S.serialize_field ctx state ~name value

  let bool (type value state output) bool
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) =
    S.serialize_bool self state bool

  let serialize_int8 (type value state output) int
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) =
    S.serialize_int8 self state int

  let serialize_int16 (type value state output) int
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) =
    S.serialize_int16 self state int

  let serialize_int31 (type value state output) int
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) =
    S.serialize_int31 self state int

  let serialize_int32 (type value state output) int
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) =
    S.serialize_int32 self state int

  let serialize_int64 (type value state output) int
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) =
    S.serialize_int64 self state int

  let serialize_float (type value state output) float
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) =
    S.serialize_float self state float

  let int int ctx = serialize_int31 int ctx
  let int8 int ctx = serialize_int8 int ctx
  let int16 int ctx = serialize_int16 int ctx
  let int32 int ctx = serialize_int32 int ctx
  let int64 int ctx = serialize_int64 int ctx
  let float float ctx = serialize_float float ctx

  let string (type value state output) string
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) =
    S.serialize_string self state string

  let option (type value state output) ser value
      (Ctx (_, (module S), state) as self : (value, state, output) ctx) =
    match value with
    | None -> S.serialize_none self state
    | Some s -> S.serialize_some self state (fun ctx -> ser s ctx)

  let list ser values ctx =
    sequence ctx (List.length values) @@ fun ctx ->
    List.fold_left
      (fun acc el -> match acc with Ok () -> element ctx (ser el) | _ -> acc)
      (Ok ()) values

  let array ser values ctx = list ser (Array.to_list values) ctx

  let s :
      type value value2 state output.
      (value, state, output) t ->
      value ->
      (value2, state, output) ctx ->
      (output, error) result =
   fun ser value (Ctx (_self, (module S), state)) ->
    let state = S.nest state in
    ser value (Ctx (ser, (module S), state))
end

module rec De_base : sig
  type 'value t = De_base.ctx -> ('value, error) result
  and ctx = Ctx : 'state De_base.deserializer * 'state -> ctx

  type ('value, 'tag) visitor = {
    visit_int : De_base.ctx -> int -> ('value, error) result;
    visit_string : De_base.ctx -> string -> ('value, error) result;
  }

  val deserializer : 'value t -> 'value t

  module type Deserializer = sig
    type state

    val nest : state -> state

    val deserialize_sequence :
      ctx ->
      state ->
      size:int ->
      (size:int -> 'value t) ->
      ('value, error) result

    val deserialize_element :
      ctx -> state -> 'value t -> ('value option, error) result

    val deserialize_variant :
      ctx ->
      state ->
      (ctx -> ('value, error) result) ->
      name:string ->
      variants:string list ->
      ('value, error) result

    val deserialize_unit_variant : ctx -> state -> (unit, error) result

    val deserialize_newtype_variant :
      ctx -> state -> 'value t -> ('value, error) result

    val deserialize_tuple_variant :
      ctx ->
      state ->
      size:int ->
      (size:int -> 'value t) ->
      ('value, error) result

    val deserialize_record_variant :
      ctx ->
      state ->
      size:int ->
      (size:int -> 'value t) ->
      ('value, error) result

    val deserialize_record :
      ctx ->
      state ->
      name:string ->
      size:int ->
      'value t ->
      ('value, error) result

    val deserialize_field :
      ctx -> state -> name:string -> 'value t -> ('value, error) result

    val deserialize_key :
      ctx -> state -> ('value, 'tag) visitor -> ('value option, error) result

    val deserialize_identifier :
      ctx -> state -> ('value, 'tag) visitor -> ('value, error) result

    val deserialize_string : ctx -> state -> (string, error) result
    val deserialize_int8 : ctx -> state -> (char, error) result
    val deserialize_int16 : ctx -> state -> (int, error) result
    val deserialize_int31 : ctx -> state -> (int, error) result
    val deserialize_int32 : ctx -> state -> (int32, error) result
    val deserialize_int64 : ctx -> state -> (int64, error) result
    val deserialize_bool : ctx -> state -> (bool, error) result
    val deserialize_float : ctx -> state -> (float, error) result

    val deserialize_option :
      ctx -> state -> 'value t -> ('value option, error) result

    val deserialize_ignored_any : ctx -> state -> (unit, error) result
  end

  type 'state deserializer = (module Deserializer with type state = 'state)
end = struct
  type 'value t = ctx -> ('value, error) result
  and ctx = Ctx : 'state De_base.deserializer * 'state -> ctx

  type ('value, 'tag) visitor = {
    visit_int : ctx -> int -> ('value, error) result;
    visit_string : ctx -> string -> ('value, error) result;
  }

  let deserializer fn = fn

  module type Deserializer = sig
    type state

    val nest : state -> state

    val deserialize_sequence :
      ctx ->
      state ->
      size:int ->
      (size:int -> 'value t) ->
      ('value, error) result

    val deserialize_element :
      ctx -> state -> 'value t -> ('value option, error) result

    val deserialize_variant :
      ctx ->
      state ->
      (ctx -> ('value, error) result) ->
      name:string ->
      variants:string list ->
      ('value, error) result

    val deserialize_unit_variant : ctx -> state -> (unit, error) result

    val deserialize_newtype_variant :
      ctx -> state -> 'value t -> ('value, error) result

    val deserialize_tuple_variant :
      ctx ->
      state ->
      size:int ->
      (size:int -> 'value t) ->
      ('value, error) result

    val deserialize_record_variant :
      ctx ->
      state ->
      size:int ->
      (size:int -> 'value t) ->
      ('value, error) result

    val deserialize_record :
      ctx ->
      state ->
      name:string ->
      size:int ->
      'value t ->
      ('value, error) result

    val deserialize_field :
      ctx -> state -> name:string -> 'value t -> ('value, error) result

    val deserialize_key :
      ctx -> state -> ('value, 'tag) visitor -> ('value option, error) result

    val deserialize_identifier :
      ctx -> state -> ('value, 'tag) visitor -> ('value, error) result

    val deserialize_string : ctx -> state -> (string, error) result
    val deserialize_int8 : ctx -> state -> (char, error) result
    val deserialize_int16 : ctx -> state -> (int, error) result
    val deserialize_int31 : ctx -> state -> (int, error) result
    val deserialize_int32 : ctx -> state -> (int32, error) result
    val deserialize_int64 : ctx -> state -> (int64, error) result
    val deserialize_bool : ctx -> state -> (bool, error) result
    val deserialize_float : ctx -> state -> (float, error) result

    val deserialize_option :
      ctx -> state -> 'value t -> ('value option, error) result

    val deserialize_ignored_any : ctx -> state -> (unit, error) result
  end

  type 'state deserializer = (module Deserializer with type state = 'state)
end

module Visitor = struct
  type ('value, 'tag) t = ('value, 'tag) De_base.visitor = {
    visit_int : De_base.ctx -> int -> ('value, error) result;
    visit_string : De_base.ctx -> string -> ('value, error) result;
  }

  let default =
    De_base.
      {
        visit_int = (fun _ctx _int -> Error `unimplemented);
        visit_string = (fun _ctx _str -> Error `unimplemented);
      }

  let make ?(visit_int = default.visit_int)
      ?(visit_string = default.visit_string) () =
    { visit_int; visit_string }

  let visit_string ctx t str = t.visit_string ctx str
  let visit_int ctx t str = t.visit_int ctx str
end

module De = struct
  include De_base

  let deserialize ctx de = de ctx

  let deserialize_int8 (Ctx ((module D), state) as ctx : ctx) =
    D.deserialize_int8 ctx state

  let deserialize_int16 (Ctx ((module D), state) as ctx : ctx) =
    D.deserialize_int16 ctx state

  let deserialize_int31 (Ctx ((module D), state) as ctx : ctx) =
    D.deserialize_int31 ctx state

  let deserialize_int32 (Ctx ((module D), state) as ctx : ctx) =
    D.deserialize_int32 ctx state

  let deserialize_int64 (Ctx ((module D), state) as ctx : ctx) =
    D.deserialize_int64 ctx state

  let deserialize_bool (Ctx ((module D), state) as ctx : ctx) =
    D.deserialize_bool ctx state

  let deserialize_float (Ctx ((module D), state) as ctx : ctx) =
    D.deserialize_float ctx state

  let deserialize_record (Ctx ((module D), state) as ctx : ctx) name size de =
    D.deserialize_record ctx state ~name ~size de

  let deserialize_field (Ctx ((module D), state) as ctx : ctx) name de =
    D.deserialize_field ctx state ~name de

  let deserialize_sequence (Ctx ((module D), state) as ctx : ctx) size de =
    D.deserialize_sequence ctx state ~size de

  let deserialize_element (Ctx ((module D), state) as ctx : ctx) de =
    D.deserialize_element ctx state de

  let deserialize_variant (Ctx ((module D), state) as ctx : ctx) ~de ~name
      ~variants =
    D.deserialize_variant ctx state de ~name ~variants

  let deserialize_unit_variant (Ctx ((module D), state) as ctx : ctx) =
    D.deserialize_unit_variant ctx state

  let deserialize_newtype_variant (Ctx ((module D), state) as ctx : ctx) de =
    D.deserialize_newtype_variant ctx state de

  let deserialize_tuple_variant (Ctx ((module D), state) as ctx : ctx) size de =
    D.deserialize_tuple_variant ctx state ~size de

  let deserialize_record_variant (Ctx ((module D), state) as ctx : ctx) size de
      =
    D.deserialize_record_variant ctx state ~size de

  let deserialize_key (Ctx ((module D), state) as ctx : ctx) visitor =
    D.deserialize_key ctx state visitor

  let deserialize_identifier (Ctx ((module D), state) as ctx : ctx) visitor =
    D.deserialize_identifier ctx state visitor

  let deserialize_string (Ctx ((module D), state) as ctx : ctx) =
    D.deserialize_string ctx state

  let deserialize_option (Ctx ((module D), state) as ctx : ctx) de =
    D.deserialize_option ctx state de

  let deserialize_ignored_any (Ctx ((module D), state) as ctx : ctx) =
    D.deserialize_ignored_any ctx state

  let record ctx name size de = deserialize_record ctx name size de
  let variant ctx name variants de = deserialize_variant ctx ~de ~name ~variants
  let sequence ctx de = deserialize_sequence ctx 0 de
  let bool ctx = deserialize_bool ctx
  let int ctx = deserialize_int31 ctx
  let int8 ctx = deserialize_int8 ctx
  let int16 ctx = deserialize_int16 ctx
  let int32 ctx = deserialize_int32 ctx
  let int64 ctx = deserialize_int64 ctx
  let string ctx = deserialize_string ctx
  let identifier ctx visitor = deserialize_identifier ctx visitor
  let unit_variant ctx = deserialize_unit_variant ctx
  let newtype_variant ctx de = deserialize_newtype_variant ctx de
  let tuple_variant ctx size de = deserialize_tuple_variant ctx size de
  let record_variant ctx size de = deserialize_record_variant ctx size de
  let element ctx de = deserialize_element ctx de
  let field ctx name de = deserialize_field ctx name de
  let next_field ctx visitor = deserialize_key ctx visitor
  let option de ctx = deserialize_option ctx de
  let float ctx = deserialize_float ctx

  let list de ctx =
    sequence ctx @@ fun ~size:_ ctx ->
    let rec read_elements acc =
      let* v = element ctx de in
      match v with Some s -> read_elements (s :: acc) | None -> Ok acc
    in
    let* list = read_elements [] in
    Ok (List.rev list)

  let array de ctx =
    let* list = list de ctx in
    Ok (Array.of_list list)

  let ignore_any ctx = deserialize_ignored_any ctx

  let d de (Ctx (((module D) as self), state) : ctx) =
    let state = D.nest state in
    de (Ctx (self, state))
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
 fun fmt ctx ser value -> ser value (Ctx (ser, fmt, ctx))

let deserialize :
    type value state output.
    state Deserializer.t -> state -> value De.t -> (value, error) result =
 fun fmt ctx de -> de (Ctx (fmt, ctx))
