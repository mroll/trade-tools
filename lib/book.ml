module PriceMap = Map.Make (Int)

type t = {
  buy_orders : Add.t list PriceMap.t;
  sell_orders : Add.t list PriceMap.t;
}

let best_price (order : Add.t) (book : t) =
  match order.side with
  | Add.Buy -> PriceMap.min_binding_opt book.sell_orders
  | Add.Sell -> PriceMap.max_binding_opt book.buy_orders

let insert_order (order : Add.t) (book : t) =
  let current_map, update_book =
    match order.side with
    | Buy -> (book.buy_orders, fun buy_orders -> { book with buy_orders })
    | Sell -> (book.sell_orders, fun sell_orders -> { book with sell_orders })
  in
  let existing_orders =
    Option.value ~default:[] (PriceMap.find_opt order.price current_map)
  in
  let updated_orders = existing_orders @ [ order ] in
  let updated_map = PriceMap.add order.price updated_orders current_map in
  update_book updated_map

let rec match_orders (order : Add.t) (price : int)
    (orders_at_price : Add.t list) (executions : Execution.t list) =
  match orders_at_price with
  | [] -> (order, [], executions)
  | resting_order :: rest ->
      if order.size = 0 then (order, resting_order :: rest, executions)
      else
        let fill_size = min order.size resting_order.size in
        let order' = { order with size = order.size - fill_size } in
        let resting_order' =
          { resting_order with size = resting_order.size - fill_size }
        in
        let executions' =
          Execution.make_execution order resting_order fill_size price 1234L
          :: executions
        in
        if resting_order'.size = 0 then
          match_orders order' price rest executions'
        else (order', resting_order' :: rest, executions')

let rec execute (order : Add.t) (book : t) =
  let book_side =
    match order.side with Buy -> book.sell_orders | Sell -> book.buy_orders
  in
  if order.size = 0 then (order, book_side, [])
  else
    match best_price order book with
    | None -> (order, book_side, [])
    | Some (price, orders_at_price) ->
        if Add.price_matches price order then
          let order', orders_at_price', executions_at_price =
            match_orders order price orders_at_price []
          in
          let book_side' =
            if orders_at_price' = [] then PriceMap.remove price book_side
            else PriceMap.add price orders_at_price' book_side
          in
          let book' =
            if order.side = Buy then { book with sell_orders = book_side' }
            else { book with buy_orders = book_side' }
          in
          let final_order, final_book_side, more_executions =
            execute order' book'
          in
          (final_order, final_book_side, executions_at_price @ more_executions)
        else (order, book_side, [])

let try_match_order (order : Add.t) (book : t) =
  match order.side with
  | Buy -> (
      match PriceMap.min_binding_opt book.sell_orders with
      | Some (ask_price, _) ->
          if order.price >= ask_price then
            let _, sell_orders', executions = execute order book in
            let updated_book = { book with sell_orders = sell_orders' } in
            (updated_book, executions)
          else (book, [])
      | None -> (book, []))
  | Sell -> (
      match PriceMap.max_binding_opt book.buy_orders with
      | Some (bid_price, _) ->
          if order.price <= bid_price then
            let _, buy_orders', executions = execute order book in
            let updated_book = { book with buy_orders = buy_orders' } in
            (updated_book, executions)
          else (book, [])
      | None -> (book, []))
