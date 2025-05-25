type t = {
  ticker : string;
  buy_order : Add.t;
  sell_order : Add.t;
  price : int;
  quantity : int;
  aggressor : Add.side;
  timestamp : int64;
}

val create :
  ticker:string ->
  buy_order:Add.t ->
  sell_order:Add.t ->
  price:int ->
  quantity:int ->
  aggressor:Add.side ->
  timestamp:int64 ->
  t

val make_execution : Add.t -> Add.t -> int -> int -> int64 -> t
val log_summary : t -> string -> unit
val to_json_string : t -> string
