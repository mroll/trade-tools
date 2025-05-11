open Core
open Async
open Trade_lib

let handle_order r stream_w sequencer =
  let%bind order_bytes = Order.accept_as_bytes_from_reader r in

  let%bind sequence_number = Sequencer.next sequencer in
  EndianBytes.BigEndian.set_int64 order_bytes 36 sequence_number;

  let%bind () =
    Writer.write_bytes stream_w order_bytes;
    Writer.flushed stream_w
  in
  return ()

let run_seqd port stream_host stream_port =
  let%bind session_manager = Session_manager.create (55000, 60000) in
  let sequencer = Sequencer.create 0L in

  let%bind matchd_socket =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port
         (Host_and_port.create ~host:stream_host ~port:stream_port))
  in
  let _sock, _stream_r, stream_w = matchd_socket in

  let%bind _ =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port) (fun addr r w ->
        Session_manager.handle_handshake session_manager
          (fun _addr r _w -> handle_order r stream_w sequencer)
          addr r w)
  in
  printf "[+] seqd started.\n";
  printf "[+] Listening on port %d.\n" port;
  Deferred.never ()

let () =
  Command.async ~summary:"Run the order sequencer"
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
     fun () -> run_seqd port stream_host stream_port)
  |> Command_unix.run
