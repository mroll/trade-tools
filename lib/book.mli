module PriceMap : Map.S with type key = int

type t = {
  buy_orders : Order.t list PriceMap.t;
  sell_orders : Order.t list PriceMap.t;
}

val best_price : Order.t -> t -> (int * Order.t list) option
val insert_order : Order.t -> t -> t
val try_match_order : Order.t -> t -> t * Execution.t list
