type t = {
  buy_order : Order.t;
  sell_order : Order.t;
  price : int;
  quantity : int;
  aggressor : Order.side;
  timestamp : int64;
}

val create :
  buy_order:Order.t ->
  sell_order:Order.t ->
  price:int ->
  quantity:int ->
  aggressor:Order.side ->
  timestamp:int64 ->
  t

val make_execution : Order.t -> Order.t -> int -> int -> int64 -> t
val log_summary : t -> string -> unit
val to_json_string : t -> string
