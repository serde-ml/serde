open Serde

type rank = Captain | Chief_petty_officer

let serialize_rank rank =
  Ser.(
    variant "rank"
      (match rank with
      | Captain -> constructor "Captain" []
      | Chief_petty_officer -> constructor "Chief_petty_officer" []))

let deserialize_rank =
  De.(
    variant "rank"
      [
        constructor "Captain" (fun () -> Ok Captain);
        constructor "Chief_petty_officer" [] (fun () -> Ok Chief_petty_officer);
      ])

type t = { name : string; rank : rank }

(* let serialize_t = *)
(*   Ser.record "t" Ser.[ field "name" string; field "rank" serialize_rank ] *)

(* let deserialize_t = *)
(*   De.record "t" De.[ field "name" string; field "rank" deserialize_rank ] *)

let obrien = { name = "Miles O'Brien"; rank = Chief_petty_officer }
let sisko = { name = "Benjamin Sisko"; rank = Captain }
