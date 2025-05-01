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
