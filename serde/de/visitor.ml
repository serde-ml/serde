open Error

module type Intf = Intf.Visitor_intf

module Unimplemented = struct
  let visit_bool _visitor _value = Error Unimplemented
  let visit_unit _visitor _value = Error Unimplemented
  let visit_char _visitor _value = Error Unimplemented
  let visit_int _visitor _value = Error Unimplemented
  let visit_float _visitor _value = Error Unimplemented
  let visit_string _visitor _value = Error Unimplemented
  let visit_tuple _visitor _value = Error Unimplemented
  let visit_variant _visitor _de _variant_access = Error Unimplemented
  let visit_seq _visitor _de _seq_access = Error Unimplemented
  let visit_map _visitor _de _map_access = Error Unimplemented
end
