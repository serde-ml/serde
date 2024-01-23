module Ser = Ser
module De = Serde_de

let serialize = Ser.serialize

type data = Data.t

module type Serializable = sig
  type t

  val serialize_t : t -> (data, 'error Ser.ser_error) result
end

let ( let* ) = Result.bind
