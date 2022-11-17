module type Instance = sig
  type t

  val pos : unit -> int
  val next : unit -> char option
  val peek : unit -> char option
  val drop : unit -> unit
  val read_to_end : unit -> string
  val skip_whitespace : unit -> unit
end

let from_string str =
  let module M = struct
    type t = {
      str : string;
      len : int;
      mutable idx : int;
      mutable curr_byte : char option;
    }

    let state : t = { str; len = String.length str; idx = 0; curr_byte = None }
    let read_to_end () = state.str
    let peek () = state.curr_byte
    let pos () = state.idx
    let drop () = state.curr_byte <- None

    let next () =
      if state.idx < state.len then (
        state.idx <- state.idx + 1;
        state.curr_byte <- Some (String.get state.str state.idx);
        state.curr_byte)
      else None

    let skip_whitespace () =
      let continue = ref true in
      while !continue do
        match next () with Some ' ' -> () | _ -> continue := false
      done
  end in
  (module M : Instance)
