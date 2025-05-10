open Async

type side = Buy | Sell

type t = {
  id : string;
  price : int;
  size : int;
  side : side;
  timestamp : int64;
  sequence_number : int64;
}

val price_matches : int -> t -> bool
val encode : t -> bytes
val decode : bytes -> t
val order_bytes : int
val accept_from_reader : Reader.t -> t Deferred.t
val accept_as_bytes_from_reader : Reader.t -> bytes Deferred.t
