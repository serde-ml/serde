type ('tag, 'value, 'error) t = {
  tag : unit -> ('tag, 'error Error.de_error) result;
  unit_variant : unit -> (unit, 'error Error.de_error) result;
  tuple_variant : unit -> ('value option, 'error Error.de_error) result;
  record_variant : unit -> ('value option, 'error Error.de_error) result;
}

let tag t = t.tag ()
let unit_variant t = t.unit_variant ()
let tuple_variant t = t.tuple_variant ()
let record_variant t = t.record_variant ()
