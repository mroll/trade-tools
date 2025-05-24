open Async
open Core
open Trade_lib

let make_order ticker price size side =
  let id = Uuid.to_string (Uuid_utils.gen_uuid ()) in
  let timestamp = Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) in
  {
    Order.id;
    ticker;
    price;
    size;
    side;
    timestamp = Int64.of_int64_exn (Int63.to_int64 timestamp);
    sequence_number = 0L;
  }

(* type top_of_book = { bid : float; ask : float } *)

let () =
  Strategy_intf.register
    (module struct
      (* let inventory = ref 0 *)
      let spread = 10.
      let quote_size = 10
      let ticker = "TEST"

      (* let my_top_of_book : (string, top_of_book) Hashtbl.t = *)
      (*   Hashtbl.create (module String) *)

      let init w oracle_host oracle_port =
        print_endline "[+] Boostrapping market maker agent";
        let%bind _oracle_sock, oracle_r, oracle_w =
          Tcp.connect
            (Tcp.Where_to_connect.of_host_and_port
               (Host_and_port.create ~host:oracle_host ~port:oracle_port))
        in
        let%bind () =
          Writer.write_line oracle_w ticker;
          Writer.flushed oracle_w
        in
        let%bind initial_price_result = Reader.read_line oracle_r in
        let fair_price =
          match initial_price_result with
          | `Ok initial_price ->
              Log.Global.info "Got fair price for %s of %s from oracle" ticker
                initial_price;
              Float.of_string initial_price
          | `Eof -> failwith "Could not read initial fair price for ticker"
        in
        let bid = Float.round (fair_price -. (spread /. 2.0)) |> int_of_float in
        let ask = Float.round (fair_price +. (spread /. 2.0)) |> int_of_float in
        let buy = make_order ticker bid quote_size Order.Buy in
        let sel = make_order ticker ask quote_size Order.Sell in
        let buy_buf = Order.encode buy in
        let sel_buf = Order.encode sel in
        Writer.write_bytes w buy_buf;
        Writer.write_bytes w sel_buf;
        Writer.flushed w

      let handle_event ev _w =
        Log.Global.info "Got event: %s"
          (Yojson.Safe.to_string (L3_event.yojson_of_l3_event ev))
    end)
