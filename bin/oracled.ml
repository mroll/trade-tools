open Core
open Async

let handle_price_request r w provide_price =
  let%bind result = Reader.read_line r in
  match result with
  | `Ok ticker ->
      let price = provide_price ticker in
      Writer.write_line w price;
      Writer.flushed w
  | `Eof ->
      printf "[+] client disconnected or sent invalid data\n%!";
      return ()

let run_oracle port range_start range_end =
  let price_map = Hashtbl.create (module String) in
  let provide_price ticker =
    match Hashtbl.find price_map ticker with
    | Some price -> price
    | None ->
        let price =
          Int.to_string (range_start + Random.int (range_start + range_end))
        in
        Hashtbl.set price_map ~key:ticker ~data:price;
        price
  in

  let%bind _ =
    Tcp.Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port) (fun _addr r w ->
        handle_price_request r w provide_price)
  in
  printf "[+] oracled started.\n";
  printf "[+] Listening on port %d.\n" port;
  Deferred.never ()

let () =
  Command.async ~summary:"Run the price discovery oracle"
    (let open Command.Let_syntax in
     let%map_open port =
       flag "--port" (required int)
         ~doc:"PORT Port to listen for incoming requests"
     and range_start =
       flag "--range-start" (required int)
         ~doc:"RANGE_START Bottom of the range for random price generation"
     and range_end =
       flag "--range-end" (required int)
         ~doc:"RANGE_END Top of the range for random price generation"
     in
     fun () -> run_oracle port range_start range_end)
  |> Command_unix.run
