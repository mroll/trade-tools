open Unix
open Trade_lib

(* Generate a random order *)
let random_order id : Order.t =
  let price = 95 + Random.int 10 in
  (* prices from 95 to 104 *)
  let size = 1 + Random.int 10 in
  (* sizes from 1 to 10 *)
  let side = if Random.bool () then Order.Buy else Order.Sell in
  let timestamp = Int64.of_float (Unix.gettimeofday ()) in
  { id; price; size; side; timestamp; sequence_number = -1L }

(* Send many orders over a single connection *)
let send_random_orders ~port ~count =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let server_addr = inet_addr_of_string "127.0.0.1" in
  let sockaddr = ADDR_INET (server_addr, port) in

  connect sock sockaddr;

  let oc = out_channel_of_descr sock in

  for i = 1 to count do
    let order = random_order (string_of_int i) in
    let buf = Order.encode order in
    output oc buf 0 (Bytes.length buf);
    flush oc;
    if i mod 1000 = 0 then Printf.printf "Sent %d orders\n%!" i;
    (* optional sleep to simulate network delay *)
    (* 1ms delay *)
  done;

  close_out oc

let () =
  Random.self_init ();
  let port = 4000 in
  let count = 1000000 in
  send_random_orders ~port ~count;
  Printf.printf "Finished sending %d orders!\n%!" count
