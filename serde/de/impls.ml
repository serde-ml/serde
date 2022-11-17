module Bool_visitor = struct
  include Visitor.Unimplemented

  type visitor = bool
  type value = bool
  type error = unit
  type tag = unit

  let visit_bool b = Ok b
end

module Unit_visitor = struct
  include Visitor.Unimplemented

  type tag = unit
  type visitor = unit
  type value = unit
  type error = unit

  let visit_unit () = Ok ()
end

module Char_visitor = struct
  include Visitor.Unimplemented

  type tag = unit
  type visitor = char
  type value = char
  type error = unit

  let visit_char c = Ok c
end

module Int_visitor = struct
  include Visitor.Unimplemented

  type tag = unit
  type visitor = int
  type value = int
  type error = unit

  let visit_int i = Ok i
end

module Float_visitor = struct
  include Visitor.Unimplemented

  type tag = unit
  type visitor = float
  type value = float
  type error = unit

  let visit_float f = Ok f
end

module String_visitor = struct
  include Visitor.Unimplemented

  type visitor = string
  type value = string
  type error = unit
  type tag = unit

  let visit_string str =
    Printf.printf "String_visitor.visit_string %s\n" str;
    Ok str
end
