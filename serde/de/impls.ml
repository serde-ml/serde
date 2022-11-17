module Bool_visitor = struct
  include Visitor.Unimplemented

  type visitor = bool
  type value = bool
  type error = unit
  type tag = unit
end

module Int_visitor = struct
  include Visitor.Unimplemented

  type tag = unit
  type visitor = int
  type value = int
  type error = unit
end
