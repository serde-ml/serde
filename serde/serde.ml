module Ser = Ser
module De = De

let serialize = Ser.serialize

type data = Data.t

module type Serializable = sig
  type t

  val serialize_t : t -> (data, 'error Ser.ser_error) result
end
