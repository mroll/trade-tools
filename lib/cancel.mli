open Async

type t = {
  id : string;
  typ : string;
  timestamp : int64;
  sequence_number : int64;
}

val accept_from_reader : Reader.t -> t option Deferred.t
val log_summary_from_bytes : bytes -> string -> unit
