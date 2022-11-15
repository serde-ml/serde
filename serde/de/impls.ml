module Bool_visitor = struct
  include Visitor.Unimplemented

  type visitor = bool
  type value = bool
  type error = unit
end

module Int_visitor = struct
  include Visitor.Unimplemented

  type visitor = int
  type value = int
  type error = unit
end
