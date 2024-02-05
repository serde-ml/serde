open Serde

type rank = Captain | Chief_petty_officer

let serialize_rank rank =
  Ser.(
    variant "rank"
      (match rank with
      | Captain -> constructor "Captain" []
      | Chief_petty_officer -> constructor "Chief_petty_officer" []))

(* let deserialize_rank = *)
(*   De.( *)
(*     variant "rank" *)
(*       [ *)
(*         unit_constructor "Captain" Captain; *)
(*         unit_constructor "Chief_petty_officer" Chief_petty_officer; *)
(*       ]) *)

type t = { name : string; rank : rank }

(* let serialize_t = *)
(*   Ser.record "t" Ser.[ field "name" string; field "rank" serialize_rank ] *)

(* let deserialize_t = *)
(*   De.record "t" De.[ field "name" string; field "rank" deserialize_rank ] *)

let obrien = { name = "Miles O'Brien"; rank = Chief_petty_officer }
let sisko = { name = "Benjamin Sisko"; rank = Captain }

(* module Chain = struct *)
(*   let int (type input) *)
(*       ( config, *)
(*         (module De : Deserializer.Intf with type input = input), *)
(*         (input : input) ) = *)
(*     let* int = De.deserialize_int config input in *)
(*     Ok int *)

(*   let string (type input) *)
(*       ( config, *)
(*         (module De : Deserializer.Intf with type input = input), *)
(*         (input : input) ) = *)
(*     let* string = De.deserialize_string config input in *)
(*     Ok string *)

(*   let test = *)
(*     run (fun str x -> *)
(*         Printf.printf "constructing value\n%!"; *)
(*         Ok (2112 + x, String.uppercase_ascii str)) *)
(*     |> chain string |> chain int *)

(*   let exec_test : (int * string, error) result = *)
(*     execute test *)
(*       ( Config.default, *)
(*         (module struct *)
(*           type input = unit *)

(*           let deserialize_int _config () = Ok 2112 *)
(*           let deserialize_string _config () = Ok "rush" *)

(*           let deserialize_variant _config _self _variant () = *)
(*             Error `unimplemented *)
(*         end), *)
(*         () ) *)
(* end *)
