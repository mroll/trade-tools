open Core
open Async
open Trade_lib

module TickerMap = struct
  include Core.Map.Make (String)

  let set t ~key ~data = Core.Map.set ~key ~data t
  let find t key = Core.Map.find t key
end

type t = Book.t TickerMap.t

let empty : t = TickerMap.empty

let find_book ticker exchange : Book.t =
  match TickerMap.find exchange ticker with
  | Some book -> book
  | None ->
      { buy_orders = Book.PriceMap.empty; sell_orders = Book.PriceMap.empty }

let update_book ticker book exchange =
  TickerMap.set exchange ~key:ticker ~data:book

let process_order ticker order exchange =
  let book = find_book ticker exchange in
  let updated_book, executions = Book.try_match_order order book in
  let book_after_insert =
    if order.size > 0 then Book.insert_order order updated_book
    else updated_book
  in
  (update_book ticker book_after_insert exchange, executions)

type l3_event =
  | Add of { order_id : string; side : Order.side; price : int; size : int }
  | Trade of { price : int; size : int; aggressor : Order.side }

(* TODO: Update for more order types *)
let l3_event_of_order (order : Order.t) : l3_event =
  Add
    {
      order_id = order.id;
      side = order.side;
      price = order.price;
      size = order.size;
    }

let l3_event_of_execution (execution : Execution.t) : l3_event =
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

let yojson_of_l3_event (event : l3_event) : Yojson.Safe.t =
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

let rec handle_order r exchange stream_w =
  let%bind result = Order.accept_from_reader r in
  match result with
  | Some order ->
      let ticker = "TEST" in
      let exch', executions = process_order ticker order !exchange in
      exchange := exch';

      (match !stream_w with
      | Some w ->
          write_l3_event_ndjson w (l3_event_of_feed_input (OrderInput order));
          List.iter executions ~f:(fun e ->
              write_l3_event_ndjson w
                (l3_event_of_feed_input (ExecutionInput e)))
      | None -> ());

      handle_order r exchange stream_w
  | None ->
      printf "[+] client disconnected or sent invalid data\n%!";
      return ()

let run_matchd port stream_port =
  let exchange = ref empty in
  let stream_w : Writer.t option ref = ref None in

  let%bind _ =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port stream_port) (fun _a _r w ->
        stream_w := Some w;
        Deferred.never ())
  in

  let%bind _ =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port) (fun _addr r _w ->
        handle_order r exchange stream_w)
  in
  printf "[+] matchd started.\n";
  printf "[+] Listening on port %d.\n" port;
  Deferred.never ()

let () =
  Command.async ~summary:"Run the matching engine"
    (let open Command.Let_syntax in
     let%map_open port =
       flag "--port" (required int)
         ~doc:"PORT Port to listen for incoming orders"
     and stream_port =
       flag "--stream-port" (required int)
         ~doc:"PORT Port to listen to for streaming clients"
     in
     fun () -> run_matchd port stream_port)
  |> Command_unix.run
