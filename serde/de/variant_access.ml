open Intf

type ('tag, 'value, 'error) t = ('tag, 'value, 'error) variant_access = {
  tag : unit -> ('tag, 'error Error.de_error) result;
  unit_variant : unit -> (unit, 'error Error.de_error) result;
  tuple_variant :
    len:int ->
    (module Visitor_intf with type value = 'value) ->
    ('value, 'error Error.de_error) result;
  record_variant : fields:string list -> ('value, 'error Error.de_error) result;
}

let tag t = t.tag ()
let unit_variant t = t.unit_variant ()
let tuple_variant t ~len v = t.tuple_variant ~len v
let record_variant t ~fields = t.record_variant ~fields
