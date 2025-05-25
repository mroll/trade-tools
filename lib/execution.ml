open Async

type t = {
  ticker : string;
  buy_order : Add.t;
  sell_order : Add.t;
  price : int;
  quantity : int;
  aggressor : Add.side;
  timestamp : int64;
}

let create ~(ticker : string) ~(buy_order : Add.t) ~(sell_order : Add.t)
    ~(price : int) ~(quantity : int) ~(aggressor : Add.side)
    ~(timestamp : int64) : t =
  { ticker; buy_order; sell_order; price; quantity; aggressor; timestamp }

let make_execution (o1 : Add.t) (o2 : Add.t) (quantity : int) (price : int)
    (timestamp : int64) =
  let buy_order = if o1.side = Buy then o1 else o2 in
  let sell_order = if o1.side = Sell then o1 else o2 in
  {
    ticker = o1.ticker;
    buy_order;
    sell_order;
    price;
    quantity;
    aggressor = o1.side;
    timestamp;
  }

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
