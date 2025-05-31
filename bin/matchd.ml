open Core
open Async
open Trade_lib

let broadcast_results order (results : Matching_engine.process_result) w =
  match w with
  | Some w -> (
      L3_event.write_l3_event_ndjson w
        (L3_event.l3_event_of_feed_input (OrderInput order));

      match results with
      | Executions executions ->
          List.iter executions ~f:(fun e ->
              L3_event.write_l3_event_ndjson w
                (L3_event.l3_event_of_feed_input (ExecutionInput e)))
      | CanceledAdds _canceled_order -> printf "Got cancel")
  | None -> ()

let rec handle_order engine r stream_w : unit Deferred.t =
  let%bind result = Order.accept_from_reader r in
  match result with
  | Some order ->
      let results_opt = Matching_engine.process_order engine order in

      let () =
        match results_opt with
        | Some results -> broadcast_results order results !stream_w
        | None -> ()
      in

      handle_order engine r stream_w
  | None ->
      printf "[+] client disconnected or sent invalid data\n%!";
      return ()

let run_matchd port stream_port =
  let engine = Matching_engine.create () in
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
        handle_order engine r stream_w)
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
