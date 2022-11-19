type 'buffer reader = {
  inner : 'buffer;
  pos : 'buffer -> int;
  next : 'buffer -> char option;
  peek : 'buffer -> char option;
  drop : 'buffer -> unit;
  read_to_end : 'buffer -> string;
  skip_whitespace : 'buffer -> unit;
}

type t

val pos : t -> int
val next : t -> char option
val peek : t -> char option
val drop : t -> unit
val read_to_end : t -> string
val skip_whitespace : t -> unit

val make : 'buffer reader -> t
(** create a new reader from a custom implementation of the reader interface.
*)

val from_string : string -> t
(** create a new reader from an in-memory string *)
