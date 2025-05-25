open Async

type t = Add of Add.t | Cancel of Cancel.t

val order_width : int
val accept_from_reader : Reader.t -> t option Deferred.t
val accept_as_bytes_from_reader : Reader.t -> bytes option Deferred.t
val log_summary_from_bytes : bytes -> string -> unit
