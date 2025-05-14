open Core
open Async
open Trade_lib

let rec handle_order r session_manager =
  let%bind line_result = Reader.read_line r in
  match line_result with
  | `Ok line ->
      let sessions = Session_manager.active_sessions session_manager in
      List.iter sessions ~f:(fun session ->
          don't_wait_for
            (Writer.write_line session.writer line;
             Writer.flushed session.writer));
      Log.Global.info "[+] received market event: %s" line;
      handle_order r session_manager
  | `Eof ->
      Log.Global.error "Got EOF while reading market data\n";
      return ()

let run_feedd matchd_host matchd_port stream_port =
  let%bind session_manager = Session_manager.create (58000, 60000) in

  let%bind matchd_socket =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port
         (Host_and_port.create ~host:matchd_host ~port:matchd_port))
  in
  let _sock, matchd_r, _matchd_w = matchd_socket in

  let%bind _ =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port stream_port) (fun addr r w ->
        Session_manager.handle_server_handshake session_manager
          (fun _ _ _ -> Deferred.never ())
          addr r w)
  in

  printf "[+] feedd started.\n";
  printf "[+] Listening for subscriptions on port %d.\n" stream_port;

  handle_order matchd_r session_manager

let () =
  Command.async ~summary:"Run the market data feed"
    (let open Command.Let_syntax in
     let%map_open match_port =
       flag "--matchd-port" (required int)
         ~doc:"PORT Port where market data is coming from"
     and matchd_host =
       flag "--matchd-host" (required string)
         ~doc:"HOST Host where market data is coming from"
     and stream_port =
       flag "--stream-port" (required int)
         ~doc:"PORT Port to listen to for streaming subscriptions"
     in
     fun () -> run_feedd matchd_host match_port stream_port)
  |> Command_unix.run
