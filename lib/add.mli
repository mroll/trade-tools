open Async

type side = Buy | Sell

type t = {
  id : string;
  typ : string;
  ticker : string;
  price : int;
  size : int;
  side : side;
  timestamp : int64;
  sequence_number : int64;
}

val encode : t -> bytes
val decode : bytes -> t
val side_to_string : side -> string
val side_of_string_exn : string -> side
val price_matches : int -> t -> bool
val accept_from_reader : Reader.t -> t option Deferred.t
val log_summary_from_bytes : bytes -> string -> unit
