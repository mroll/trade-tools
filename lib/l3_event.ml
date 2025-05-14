open Async

type t =
  | Add of { order_id : string; side : Order.side; price : int; size : int }
  | Trade of { price : int; size : int; aggressor : Order.side }

(* TODO: Update for more order types *)
let l3_event_of_order (order : Order.t) : t =
  Add
    {
      order_id = order.id;
      side = order.side;
      price = order.price;
      size = order.size;
    }

let l3_event_of_execution (execution : Execution.t) : t =
  Trade
    {
      price = execution.price;
      size = execution.quantity;
      aggressor = execution.aggressor;
    }

type feed_input = OrderInput of Order.t | ExecutionInput of Execution.t

let l3_event_of_feed_input = function
  | OrderInput o -> l3_event_of_order o
  | ExecutionInput e -> l3_event_of_execution e

let yojson_of_l3_event (event : t) : Yojson.Safe.t =
  match event with
  | Add { order_id; side; price; size } ->
      `Assoc
        [
          ("type", `String "add");
          ("order_id", `String order_id);
          ("side", `String (Order.side_to_string side));
          ("price", `Int price);
          ("size", `Int size);
        ]
  | Trade { price; size; aggressor } ->
      `Assoc
        [
          ("type", `String "trade");
          ("price", `Int price);
          ("size", `Int size);
          ("aggressor", `String (Order.side_to_string aggressor));
        ]

let write_l3_event_ndjson writer event =
  let json = yojson_of_l3_event event in
  let line = Yojson.Safe.to_string json in
  Writer.write_line writer line

let l3_event_of_yojson_exn (json : Yojson.Safe.t) : t =
  let open Yojson.Safe.Util in
  let kind = json |> member "type" |> to_string in
  match kind with
  | "add" ->
      let order_id = json |> member "order_id" |> to_string in
      let side =
        json |> member "side" |> to_string |> Order.side_of_string_exn
      in
      let price = json |> member "price" |> to_int in
      let size = json |> member "size" |> to_int in
      Add { order_id; side; price; size }
  | "trade" ->
      let price = json |> member "price" |> to_int in
      let size = json |> member "size" |> to_int in
      let aggressor =
        json |> member "aggressor" |> to_string |> Order.side_of_string_exn
      in
      Trade { price; size; aggressor }
  | other -> failwith (Printf.sprintf "Unknown l3_event type: %s" other)
