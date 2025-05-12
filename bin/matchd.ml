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

let rec handle_order r exchange =
  let%bind result = Order.accept_from_reader r in
  match result with
  | Some order ->
      let ticker = "TEST" in
      let exch', executions = process_order ticker order !exchange in
      exchange := exch';
      List.iter executions ~f:(fun e -> Execution.log_summary e "executed");
      handle_order r exchange
  | None ->
      printf "[+] client disconnected or sent invalid data\n%!";
      return ()

let run_matchd port =
  let exchange = ref empty in

  let%bind _ =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port) (fun _addr r _w ->
        handle_order r exchange)
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
       (* and stream_port = *)
       (*   flag "--stream-port" (required int) *)
       (*     ~doc:"PORT Port to listen to for streaming clients" *)
     in
     fun () -> run_matchd port)
  |> Command_unix.run
