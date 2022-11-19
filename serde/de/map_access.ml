open Intf

type ('value, 'error) t = ('value, 'error) map_access = {
  next_key :
    'key. deser_key:(unit -> ('key, 'error Error.de_error) result) -> ('key option, 'error Error.de_error) result;
  next_value :
    deser_value:(unit -> ('value, 'error Error.de_error) result) ->
    ('value option, 'error Error.de_error) result;
}

let next_key t ~deser_key = t.next_key ~deser_key
let next_value t ~deser_value = t.next_value ~deser_value
