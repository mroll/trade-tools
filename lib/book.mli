module PriceMap : Map.S with type key = int

type t = {
  buy_orders : Add.t list PriceMap.t;
  sell_orders : Add.t list PriceMap.t;
}

val best_price : Add.t -> t -> (int * Add.t list) option
val insert_order : Add.t -> t -> t
val try_match_order : Add.t -> t -> t * Execution.t list
