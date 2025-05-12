open Core
open Async
open Trade_lib

let rec handle_order r stream_w =
  let%bind result = Order.accept_as_bytes_from_reader r in
  match result with
  | Some order_bytes ->
      Order.log_summary_from_bytes order_bytes "[+] received order ";
      Writer.write_bytes stream_w order_bytes;
      let%bind () = Writer.flushed stream_w in
      handle_order r stream_w
  | None ->
      printf "[+] client disconnected or sent invalid data\n%!";
      return ()

let run_gateway port stream_host stream_port =
  let%bind session_manager = Session_manager.create (50000, 54999) in

  let%bind sequencer_session_port =
    Session_manager.handle_client_handshake stream_host stream_port
  in
  let%bind sequencer_socket =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port
         (Host_and_port.create ~host:stream_host ~port:sequencer_session_port))
  in
  let _sock, _stream_r, stream_w = sequencer_socket in

  let%bind _ =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port) (fun addr r w ->
        Session_manager.handle_server_handshake session_manager
          (fun _addr r _w -> handle_order r stream_w)
          addr r w)
  in
  printf "[+] routd started.\n";
  printf "[+] Listening on port %d.\n" port;
  Deferred.never ()

let () =
  Command.async ~summary:"Run the public-facing trading gateway"
    (let open Command.Let_syntax in
     let%map_open port =
       flag "--port" (required int)
         ~doc:"PORT Port to listen for incoming orders"
     and stream_host =
       flag "--stream-host" (required string)
         ~doc:"HOST Address of the downstream service to send orders to"
     and stream_port =
       flag "--stream-port" (required int)
         ~doc:"PORT Port on the stream host to connect to"
     in
     fun () -> run_gateway port stream_host stream_port)
  |> Command_unix.run
