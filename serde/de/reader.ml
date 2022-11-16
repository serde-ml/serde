module type Intf = sig
  type t

  val read_to_string : unit -> string
end

let from_string t =
  let module M = struct
    type t = string

    let read_to_string () = t
  end in
  (module M : Intf)
