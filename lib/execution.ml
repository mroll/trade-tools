open Async

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

let log_summary execution prefix =
  Log.Global.info
    "%sbuy_order=%s sell_order=%s price=%d quantity=%d timestamp=%s" prefix
    execution.buy_order.id execution.sell_order.id execution.price
    execution.quantity
    (Int64.to_string execution.timestamp)

let to_json (e : t) : Yojson.Safe.t =
  `Assoc
    [
      ("type", `String "execution");
      ("buy_order", `String e.buy_order.id);
      ("sell_order", `String e.sell_order.id);
      ("price", `Int e.price);
      ("quantity", `Int e.quantity);
      ("timestamp", `String (Int64.to_string e.timestamp));
    ]

let to_json_string (e : t) : string = Yojson.Safe.to_string (to_json e)
