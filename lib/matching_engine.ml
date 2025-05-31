open Core
open Async

type t = {
  books : (string, Book.t) Hashtbl.t;
  orders : (string, Add.t) Hashtbl.t;
}

type process_result =
  | Executions of Execution.t list
  | CanceledAdds of Add.t option

let find_book t (order_id : string) : Book.t =
  let order = Hashtbl.find_exn t.orders order_id in
  match Hashtbl.find t.books order.ticker with
  | Some book -> book
  | None ->
      { buy_orders = Book.PriceMap.empty; sell_orders = Book.PriceMap.empty }

let update_book t ticker book = Hashtbl.set t.books ~key:ticker ~data:book

let create () : t =
  {
    books = Hashtbl.create (module String);
    orders = Hashtbl.create (module String);
  }

let process_order t order : process_result option =
  match order with
  | Order.Add add ->
      let book = find_book t add.ticker in
      let updated_book, executions = Book.try_match_order add book in
      let book_after_insert =
        if add.size > 0 then Book.insert_order add updated_book
        else updated_book
      in
      update_book t add.ticker book_after_insert;
      Some (Executions executions)
  | Order.Cancel cancel -> (
      let book = find_book t cancel.order_id in
      match Hashtbl.find t.orders cancel.order_id with
      | Some add -> (
          let updated_book, found = Book.cancel book add in
          match found with
          | true ->
              Hashtbl.set t.books ~key:add.ticker ~data:updated_book;
              Some (CanceledAdds (Some add))
          | false ->
              Log.Global.info "No order to cancel";
              None)
      | None ->
          Log.Global.info "No order to cancel";
          None)
