open Core

module Uuid_key = struct
  type t = Uuid.t [@@deriving sexp_of]

  let hash = Hashtbl.hash
  let compare = Uuid.compare
end

let rng = Random.State.make_self_init ()
let gen_uuid () = Uuid.create_random rng
