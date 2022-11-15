module type Intf = sig
  type t

  val read_to_string : t -> unit -> string
end

module Buffer : sig
  include Intf

  val make : Buffer.t -> t
end = struct
  type t = Buffer.t

  let make t = t
  let read_to_string t () = Buffer.contents t
end

module String : sig
  include Intf

  val make : string -> t
end = struct
  type t = string

  let make t = t
  let read_to_string t () = t
end
