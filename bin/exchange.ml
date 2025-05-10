open Cmdliner
open Trade_lib
module TickerMap = Map.Make (String)

type t = Book.t TickerMap.t

let port =
  let doc = "Port to listen for incoming trades." in
  Arg.(value & opt int 4000 & info [ "p"; "port" ] ~docv:"PORT" ~doc)

(* let log_file = *)
(*   let doc = "File to append public trade log to." in *)
(*   Arg.( *)
(*     value & opt (some string) None & info [ "l"; "log" ] ~docv:"LOG_FILE" ~doc) *)

let bind_address =
  let doc = "IP address to bind the server to (default 127.0.0.1)." in
  Arg.(
    value & opt string "127.0.0.1"
    & info [ "b"; "bind" ] ~docv:"BIND_ADDRESS" ~doc)

let empty : t = TickerMap.empty

let find_book ticker exchange : Book.t =
  match TickerMap.find_opt ticker exchange with
  | Some book -> book
  | None ->
      { buy_orders = Book.PriceMap.empty; sell_orders = Book.PriceMap.empty }

let update_book ticker book exchange = TickerMap.add ticker book exchange

let process_order ticker order exchange =
  let book = find_book ticker exchange in
  let updated_book, executions = Book.try_match_order order book in
  let book_after_insert =
    if order.size > 0 then Book.insert_order order updated_book
    else updated_book
  in
  (update_book ticker book_after_insert exchange, executions)

let rec handle_connection ic oc exchange =
  let buf = Bytes.create Order.order_bytes in
  (* 4. Accept loop *)
  try
    really_input ic buf 0 Order.order_bytes;
    let order = Order.decode buf in

    let ticker = "TEST" in
    let exch', _ = process_order ticker order !exchange in
    exchange := exch';

    output_string oc "OK\n";
    flush oc;
    handle_connection ic oc exchange
  with exn ->
    Printf.eprintf "Error processing request: %s\n%!" (Printexc.to_string exn);

    close_in_noerr ic;
    close_out_noerr oc

let rec serve sock exchange =
  let client_sock, _client_addr = Unix.accept sock in
  let ic = Unix.in_channel_of_descr client_sock in
  let oc = Unix.out_channel_of_descr client_sock in
  handle_connection ic oc exchange;
  Printf.eprintf "Re-serving\n";
  serve sock exchange

let run_exchange port bind_address =
  (* 1. Create the initial exchange *)
  let exchange = ref empty in

  (* 3. Set up server socket *)
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let inet_addr = Unix.inet_addr_of_string bind_address in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  Unix.setsockopt sock SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 10;

  (* 10 pending connections allowed *)
  Printf.printf "Exchange server listening on %s:%d\n%!" bind_address port;

  serve sock exchange

let cmd =
  let doc = "Run the exchange server" in
  let exits = Cmd.Exit.defaults in
  let term = Term.(const run_exchange $ port $ bind_address) in
  Cmd.v (Cmd.info "exchange" ~doc ~exits) term

let () = exit (Cmd.eval cmd)
