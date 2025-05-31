open Core

type t = {
  books : (string, Book.t) Hashtbl.t;
  orders : (string, Add.t) Hashtbl.t;
}

type process_result =
  | Executions of Execution.t list
  | CanceledAdds of Add.t option

val create : unit -> t
val process_order : t -> Order.t -> process_result option
