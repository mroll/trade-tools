open Core

type t = {
  books : (string, Book.t) Hashtbl.t;
  orders : (string, Add.t) Hashtbl.t;
}

type process_result = Execution of Execution.t | CanceledAdd of Add.t

val create : unit -> t
val process_order : t -> Order.t -> process_result list
