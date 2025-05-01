type t = {
  buy_order : Order.t;
  sell_order : Order.t;
  price : int;
  quantity : int;
  timestamp : int64;
}

let create ~(buy_order : Order.t) ~(sell_order : Order.t) ~(price : int)
    ~(quantity : int) ~(timestamp : int64) : t =
  { buy_order; sell_order; price; quantity; timestamp }

let make_execution (o1 : Order.t) (o2 : Order.t) (quantity : int) (price : int)
    (timestamp : int64) =
  let buy_order = if o1.side = Buy then o1 else o2 in
  let sell_order = if o1.side = Sell then o1 else o2 in
  { buy_order; sell_order; price; quantity; timestamp }
