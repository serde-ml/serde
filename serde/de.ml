type 'err error =
  | Unimplemented
  | Invalid_type of { err_expected : string; err_unexpected : string }
  | Custom of 'err

type ('visitor, 'value, 'error) de = {
  deserialize_any :
    ('visitor, 'value, 'error) de -> 'visitor -> ('value, 'error error) result;
}

let default = { deserialize_any = (fun _ _ -> Error Unimplemented) }

type t
