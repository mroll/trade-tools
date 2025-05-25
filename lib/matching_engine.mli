open Core

type t = (string, Book.t) Hashtbl.t

val create : unit -> t
val process_order : t -> Add.t -> Execution.t List.t
