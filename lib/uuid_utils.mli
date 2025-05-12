module Uuid_key : sig
  type t = Uuid.t [@@deriving sexp_of]

  val hash : t -> int
  val compare : t -> t -> int
end

val gen_uuid : unit -> Uuid_key.t
