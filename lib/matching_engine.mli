open Core

type t = (string, Book.t) Hashtbl.t

val create : unit -> t
val process_order : t -> Order.t -> Execution.t List.t
