open Core
open Async
open Trade_lib

let generate_order ~sequence_number =
  let ticker = "TEST" in
  let price = Random.int 1000 + 100 in
  let size = Random.int 100 + 1 in
  let side = if Random.bool () then Add.Buy else Add.Sell in
  let id = Uuid.to_string (Uuid_utils.gen_uuid ()) in
  let timestamp = Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) in

  {
    Add.id;
    typ = "add";
    ticker;
    price;
    size;
    side;
    timestamp = Int64.of_int64_exn (Int63.to_int64 timestamp);
    sequence_number;
  }

let rec send_orders writer ~count ~seq =
  (* printf "Sending order %d\n" count; *)
  if count <= 0 then return ()
  else
    let order = generate_order ~sequence_number:seq in
    let buf = Add.encode order in
    Writer.write_bytes writer buf;
    let%bind () = Writer.flushed writer in
    (* let%bind () = after (Time_float.Span.of_ms 2.) in *)
    send_orders writer ~count:(count - 1) ~seq:Int64.(seq + 1L)

let run ~host ~port ~count =
  let%bind session_port = Session_manager.handle_client_handshake host port in

  let%bind socket =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port
         (Host_and_port.create ~host ~port:session_port))
  in
  let _sock, _reader, writer = socket in

  let%bind () = send_orders writer ~count ~seq:0L in
  printf "Finished sending %d simulated orders\n%!" count;
  return ()

let () =
  Command.async ~summary:"Simulate sending orders to the exchange"
    (let open Command.Let_syntax in
     let%map_open host =
       flag "--host" (required string) ~doc:"HOST Exchange address"
     and port = flag "--port" (required int) ~doc:"PORT Exchange TCP port"
     and count =
       flag "--count"
         (optional_with_default 100 int)
         ~doc:"N Number of orders to send"
     in
     fun () -> run ~host ~port ~count)
  |> Command_unix.run
