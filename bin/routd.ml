open Core
open Async
open Trade_lib

let handle_order r stream_w =
  let%bind order = Order.accept_as_bytes_from_reader r in

  let%bind () =
    Writer.write_bytes stream_w order;
    Writer.flushed stream_w
  in
  return ()

let handle_handshake used_ports f =
  let allocate_port start stop used_ports =
    let rec find p =
      if p > stop then None
      else if Set.mem used_ports p then find (p + 1)
      else Some p
    in
    find start
  in

  fun _addr _r w ->
    match allocate_port 50000 60000 !used_ports with
    | Some new_port ->
        used_ports := Set.add !used_ports new_port;

        let%bind _server =
          Tcp.Server.create ~on_handler_error:`Raise
            (Tcp.Where_to_listen.of_port new_port) (fun client_addr r w ->
              let%bind () = f client_addr r w in
              return ())
        in

        let%bind () =
          Writer.write_line w (Int.to_string new_port);
          Writer.flushed w
        in
        return ()
    | None ->
        let%bind () =
          Writer.write_line w "ERROR: No ports available";
          Writer.flushed w
        in
        return ()

let run_gateway port stream_host stream_port =
  let used_ports = ref Int.Set.empty in

  let%bind sequencer_socket =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port
         (Host_and_port.create ~host:stream_host ~port:stream_port))
  in
  let _sock, _stream_r, stream_w = sequencer_socket in

  let%bind _ =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (handle_handshake used_ports (fun _client_add r _w ->
           handle_order r stream_w))
  in
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
