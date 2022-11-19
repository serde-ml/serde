type 'buffer reader = {
  inner : 'buffer;
  pos : 'buffer -> int;
  next : 'buffer -> char option;
  peek : 'buffer -> char option;
  drop : 'buffer -> unit;
  read_to_end : 'buffer -> string;
  skip_whitespace : 'buffer -> unit;
}

type t = Reader : 'buffer reader -> t

let make reader = Reader reader
let pos (Reader { inner; pos; _ }) = pos inner
let next (Reader { inner; next; _ }) = next inner
let peek (Reader { inner; peek; _ }) = peek inner
let drop (Reader { inner; drop; _ }) = drop inner
let read_to_end (Reader { inner; read_to_end; _ }) = read_to_end inner
let skip_whitespace (Reader { inner; skip_whitespace = sk; _ }) = sk inner

module String = struct
  type t = { str : string; len : int; mutable idx : int }

  let read_to_end state = state.str

  let peek state =
    if state.idx < state.len then Some state.str.[state.idx] else None

  let pos state = state.idx
  let drop state = state.idx <- state.idx + 1

  let next state =
    drop state;
    peek state

  let skip_whitespace state =
    let continue = ref true in
    while !continue do
      match peek state with Some ' ' -> drop state | _ -> continue := false
    done

  let make str =
    let state = { str; len = String.length str; idx = 0 } in
    Reader
      { inner = state; read_to_end; peek; pos; drop; next; skip_whitespace }
end

let from_string str = String.make str
