open Core
open Async
open Trade_lib

let _ = ignore (Trade_lib.Uuid_utils.gen_uuid ())

let rec handle_market_event data_r gateway_w strategy_handle_event =
  let%bind result = Reader.read_line data_r in
  match result with
  | `Ok line ->
      let event =
        L3_event.l3_event_of_yojson_exn (Yojson.Safe.from_string line)
      in
      strategy_handle_event event gateway_w;
      handle_market_event data_r gateway_w strategy_handle_event
  | `Eof ->
      Log.Global.error "Got EOF while reading market data\n";
      return ()

let run_agentd gateway_host gateway_port data_host data_port oracle_host
    oracle_port strategy_path =
  Dynlink.loadfile_private strategy_path;
  let module S = (val Strategy_intf.get () : Strategy_intf.STRATEGY) in
  let%bind gateway_session_port =
    Session_manager.handle_client_handshake gateway_host gateway_port
  in
  let%bind gateway_socket =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port
         (Host_and_port.create ~host:gateway_host ~port:gateway_session_port))
  in
  let _gateway_sock, _gateway_r, gateway_w = gateway_socket in

  let%bind data_session_port =
    Session_manager.handle_client_handshake data_host data_port
  in
  let%bind data_socket =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port
         (Host_and_port.create ~host:data_host ~port:data_session_port))
  in
  let _data_sock, data_r, _data_w = data_socket in

  printf "[+] agentd started.\n";
  printf "[+] Connected to trading gateway on port %d.\n" gateway_port;
  printf "[+] Listening for market data on port %d.\n" data_port;

  let%bind () = S.init gateway_w oracle_host oracle_port in
  handle_market_event data_r gateway_w S.handle_event

let () =
  Command.async ~summary:"Run a trading strategy"
    (let open Command.Let_syntax in
     let%map_open gateway_host =
       flag "--gateway-host" (required string)
         ~doc:"HOST Host where the trading gateway is"
     and gateway_port =
       flag "--gateway-port" (required int)
         ~doc:"PORT Port to connect to the gateway on"
     and data_host =
       flag "--data-host" (required string)
         ~doc:"HOST Host where the market data is"
     and data_port =
       flag "--data-port" (required int)
         ~doc:"PORT Port to listen to for market data"
     and oracle_host =
       flag "--oracle-host" (required string)
         ~doc:"HOST Host where the oracle is"
     and oracle_port =
       flag "--oracle-port" (required int)
         ~doc:"PORT Port to connect to oracle on"
     and strategy_path =
       flag "--strategy" (required string)
         ~doc:"PATH Path to plugin file with strategy logic"
     in
     fun () ->
       run_agentd gateway_host gateway_port data_host data_port oracle_host
         oracle_port strategy_path)
  |> Command_unix.run
