open Core
open Async

type side = Buy | Sell

let typ_width = 3

type t = {
  id : string;
  typ : string;
  ticker : string;
  price : int;
  size : int;
  side : side;
  timestamp : int64;
  sequence_number : int64;
}

(* let width = 46 *)
let side_to_byte = function Buy -> 0 | Sell -> 1

let byte_to_side = function
  | 0 -> Buy
  | 1 -> Sell
  | _ -> failwith "Invalid side byte"

let side_to_string = function Buy -> "buy" | Sell -> "sell"

let price_matches (price : int) (add : t) =
  match add.side with Buy -> add.price >= price | Sell -> add.price <= price

let side_of_string_exn (s : string) : side =
  match String.lowercase s with
  | "buy" -> Buy
  | "sell" -> Sell
  | _ -> invalid_arg ("Order.side_of_string_exn: unknown side: " ^ s)

(* let to_json (add : t) : Yojson.Safe.t = *)
(*   `Assoc *)
(*     [ *)
(*       ("id", `String add.id); *)
(*       ("type", `String "add"); *)
(*       ("ticker", `String "ticker"); *)
(*       ("price", `Int add.price); *)
(*       ("size", `Int add.size); *)
(*       ("side", `String (side_to_string add.side)); *)
(*       ("timestamp", `String (Int64.to_string add.timestamp)); *)
(*       ("sequence_number", `String (Int64.to_string add.sequence_number)); *)
(*     ] *)

(* let to_json_string (o : t) : string = Yojson.Safe.to_string (to_json o) *)

let encode (o : t) : bytes =
  let buf = Bytes.make 49 '\000' in
  (* type 3 bytes *)
  let typ_bytes = Bytes.of_string o.typ in
  Bytes.blit ~src:typ_bytes ~src_pos:0 ~dst:buf ~dst_pos:0 ~len:typ_width;

  (* id (16 bytes, padded) *)
  let id_bytes = Bytes.of_string o.id in
  Bytes.blit ~src:id_bytes ~src_pos:0 ~dst:buf ~dst_pos:typ_width
    ~len:(min 16 (Bytes.length id_bytes));

  (* ticker *)
  let ticker_bytes = Bytes.of_string o.ticker in
  Bytes.blit ~src:ticker_bytes ~src_pos:0 ~dst:buf ~dst_pos:19
    ~len:(min 5 (Bytes.length ticker_bytes));

  (* price and size *)
  let () =
    match Int32.of_int o.price with
    | Some price -> EndianBytes.BigEndian.set_int32 buf 24 price
    | None -> failwith "Failed to encode order price"
  in

  let () =
    match Int32.of_int o.size with
    | Some size -> EndianBytes.BigEndian.set_int32 buf 28 size
    | None -> failwith "Failed to encode order size"
  in

  (* side *)
  let () =
    match Char.of_int (side_to_byte o.side) with
    | Some side_bytes -> Bytes.set buf 32 side_bytes
    | None -> failwith "Failed to encode side"
  in

  (* timestamp *)
  EndianBytes.BigEndian.set_int64 buf 33 o.timestamp;

  (* sequence number *)
  EndianBytes.BigEndian.set_int64 buf 41 o.sequence_number;

  buf

let decode (buf : bytes) : t =
  if Bytes.length buf <> 46 then failwith "Invalid buffer length";

  let id = Bytes.sub buf ~pos:0 ~len:16 |> Bytes.to_string |> String.strip in
  let ticker =
    Bytes.sub buf ~pos:16 ~len:5 |> Bytes.to_string |> String.strip
  in
  let price = EndianBytes.BigEndian.get_int32 buf 21 |> Int32.to_int_exn in
  let size = EndianBytes.BigEndian.get_int32 buf 25 |> Int32.to_int_exn in
  let side = Bytes.get buf 29 |> Char.to_int |> byte_to_side in
  let timestamp = EndianBytes.BigEndian.get_int64 buf 30 in
  let sequence_number = EndianBytes.BigEndian.get_int64 buf 38 in

  { id; typ = "add"; ticker; price; size; side; timestamp; sequence_number }

let log_summary_from_bytes buf prefix =
  let id = Bytes.sub buf ~pos:0 ~len:16 |> Bytes.to_string |> String.strip in
  let ticker =
    Bytes.sub buf ~pos:16 ~len:5 |> Bytes.to_string |> String.strip
  in
  let price = EndianBytes.BigEndian.get_int32 buf 16 |> Int32.to_int_exn in
  let size = EndianBytes.BigEndian.get_int32 buf 20 |> Int32.to_int_exn in
  let side = Bytes.get buf 24 |> Char.to_int in
  let side_str = match side with 0 -> "Buy" | 1 -> "Sell" | _ -> "?" in
  let sequence_number = EndianBytes.BigEndian.get_int64 buf 36 in

  Log.Global.info
    "%sid=%s type=add ticker=%s sequence_number=%s size=%d price=%d side=%s"
    prefix id ticker
    (Int64.to_string sequence_number)
    size price side_str

let accept_from_reader r =
  let buf = Bytes.create 46 in
  let%bind result = Reader.really_read r buf ~pos:0 ~len:46 in
  match result with
  | `Eof n when n = 0 -> return None
  | `Eof n ->
      printf "Partial read (got %d bytes), closing connection\n%!" n;
      return None
  | `Ok ->
      let add = decode buf in
      return (Some add)
