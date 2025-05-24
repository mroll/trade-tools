open Core

type t = (string, Book.t) Hashtbl.t

let find_book t (order : Order.t) : Book.t =
  match Hashtbl.find t order.ticker with
  | Some book -> book
  | None ->
      { buy_orders = Book.PriceMap.empty; sell_orders = Book.PriceMap.empty }

let update_book t ticker book = Hashtbl.set t ~key:ticker ~data:book

let process_order t order =
  let book = find_book t order in
  let updated_book, executions = Book.try_match_order order book in
  let book_after_insert =
    if order.size > 0 then Book.insert_order order updated_book
    else updated_book
  in
  update_book t order.ticker book_after_insert;
  executions

let create () : t = Hashtbl.create (module String)
