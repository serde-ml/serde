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
    type t = { str : string; len : int; mutable idx : int }

    let state : t = { str; len = String.length str; idx = 0 }
    let read_to_end () = state.str

    let peek () =
      if state.idx < state.len then Some state.str.[state.idx] else None

    let pos () = state.idx
    let drop () = state.idx <- state.idx + 1

    let next () =
      drop ();
      peek ()

    let skip_whitespace () =
      let continue = ref true in
      while !continue do
        match peek () with Some ' ' -> drop () | _ -> continue := false
      done
  end in
  (module M : Instance)
