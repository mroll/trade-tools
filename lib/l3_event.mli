open Async

(** Type representing a single L3 market event *)
type t =
  | Add of {
      order_id : string;
      ticker : string;
      side : Add.side;
      price : int;
      size : int;
    }
  | Cancel of { order_id : string }
  | Trade of { ticker : string; price : int; size : int; aggressor : Add.side }

(** Type representing abstract input to the feed — either an order or an
    execution *)
type feed_input = OrderInput of Order.t | ExecutionInput of Execution.t

val l3_event_of_add : Add.t -> t

val l3_event_of_cancel : Cancel.t -> t
(** Convert an Order.t into an L3 add event *)

val l3_event_of_execution : Execution.t -> t
(** Convert an Execution.t into an L3 trade event *)

val l3_event_of_feed_input : feed_input -> t
(** Convert a generic feed input (order or execution) into an L3 event *)

val yojson_of_l3_event : t -> Yojson.Safe.t
(** Convert an L3 event to Yojson.Safe.t for NDJSON output *)

val write_l3_event_ndjson : Writer.t -> t -> unit
(** Serialize and write an L3 event to a Writer as a line of NDJSON *)

val l3_event_of_yojson_exn : Yojson.Safe.t -> t
