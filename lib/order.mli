open Async

type side = Buy | Sell

type t = {
  id : string;
  ticker : string;
  price : int;
  size : int;
  side : side;
  timestamp : int64;
  sequence_number : int64;
}

val side_of_string_exn : string -> side
val side_to_string : side -> string
val to_json_string : t -> string
val price_matches : int -> t -> bool
val encode : t -> bytes
val decode : bytes -> t
val order_width : int
val accept_from_reader : Reader.t -> t option Deferred.t
val accept_as_bytes_from_reader : Reader.t -> bytes option Deferred.t
val log_summary_from_bytes : bytes -> string -> unit
