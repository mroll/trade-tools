open Async

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

(* TODO: Update for more order types *)
let l3_event_of_add (add : Add.t) : t =
  Add
    {
      order_id = add.id;
      ticker = add.ticker;
      side = add.side;
      price = add.price;
      size = add.size;
    }

let l3_event_of_cancel (cancel : Cancel.t) : t = Cancel { order_id = cancel.id }

let l3_event_of_execution (execution : Execution.t) : t =
  Trade
    {
      ticker = execution.ticker;
      price = execution.price;
      size = execution.quantity;
      aggressor = execution.aggressor;
    }

type feed_input = OrderInput of Order.t | ExecutionInput of Execution.t

let l3_event_of_feed_input = function
  | OrderInput o -> (
      match o with
      | Add add -> l3_event_of_add add
      | Cancel cancel -> l3_event_of_cancel cancel)
  | ExecutionInput e -> l3_event_of_execution e

let yojson_of_l3_event (event : t) : Yojson.Safe.t =
  match event with
  | Add { order_id; ticker; side; price; size } ->
      `Assoc
        [
          ("type", `String "add");
          ("ticker", `String ticker);
          ("order_id", `String order_id);
          ("side", `String (Add.side_to_string side));
          ("price", `Int price);
          ("size", `Int size);
        ]
  | Cancel { order_id } ->
      `Assoc [ ("type", `String "cancel"); ("order_id", `String order_id) ]
  | Trade { ticker; price; size; aggressor } ->
      `Assoc
        [
          ("type", `String "trade");
          ("ticker", `String ticker);
          ("price", `Int price);
          ("size", `Int size);
          ("aggressor", `String (Add.side_to_string aggressor));
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
      let ticker = json |> member "ticker" |> to_string in
      let side = json |> member "side" |> to_string |> Add.side_of_string_exn in
      let price = json |> member "price" |> to_int in
      let size = json |> member "size" |> to_int in
      Add { order_id; ticker; side; price; size }
  | "trade" ->
      let price = json |> member "price" |> to_int in
      let ticker = json |> member "ticker" |> to_string in
      let size = json |> member "size" |> to_int in
      let aggressor =
        json |> member "aggressor" |> to_string |> Add.side_of_string_exn
      in
      Trade { ticker; price; size; aggressor }
  | other -> failwith (Printf.sprintf "Unknown l3_event type: %s" other)
