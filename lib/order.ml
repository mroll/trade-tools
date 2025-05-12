open Core
open Async

type side = Buy | Sell

type t = {
  id : string;
  price : int;
  size : int;
  side : side;
  timestamp : int64;
  sequence_number : int64;
}

let order_bytes = 44

let price_matches (price : int) (order : t) =
  match order.side with
  | Buy -> order.price >= price
  | Sell -> order.price <= price

let side_to_byte = function Buy -> 0 | Sell -> 1

let byte_to_side = function
  | 0 -> Buy
  | 1 -> Sell
  | _ -> failwith "Invalid side byte"

let encode (o : t) : bytes =
  let buf = Bytes.make 44 '\000' in
  (* id (16 bytes, padded) *)
  let id_bytes = Bytes.of_string o.id in
  Bytes.blit ~src:id_bytes ~src_pos:0 ~dst:buf ~dst_pos:0
    ~len:(min 16 (Bytes.length id_bytes));

  (* price and size *)
  let () =
    match Int32.of_int o.price with
    | Some price -> EndianBytes.BigEndian.set_int32 buf 16 price
    | None -> failwith "Failed to encode order price"
  in

  let () =
    match Int32.of_int o.size with
    | Some size -> EndianBytes.BigEndian.set_int32 buf 20 size
    | None -> failwith "Failed to encode order size"
  in

  (* side *)
  let () =
    match Char.of_int (side_to_byte o.side) with
    | Some side_bytes -> Bytes.set buf 24 side_bytes
    | None -> failwith "Failed to encode side"
  in

  (* 3 bytes padding: already \000 *)

  (* timestamp *)
  EndianBytes.BigEndian.set_int64 buf 28 o.timestamp;

  (* sequence number *)
  EndianBytes.BigEndian.set_int64 buf 36 o.sequence_number;

  buf

let decode (buf : bytes) : t =
  if Bytes.length buf <> 44 then failwith "Invalid buffer length";

  let id = Bytes.sub buf ~pos:0 ~len:16 |> Bytes.to_string |> String.strip in
  let price = EndianBytes.BigEndian.get_int32 buf 16 |> Int32.to_int_exn in
  let size = EndianBytes.BigEndian.get_int32 buf 20 |> Int32.to_int_exn in
  let side = Bytes.get buf 24 |> Char.to_int |> byte_to_side in
  let timestamp = EndianBytes.BigEndian.get_int64 buf 28 in
  let sequence_number = EndianBytes.BigEndian.get_int64 buf 36 in

  { id; price; size; side; timestamp; sequence_number }

let accept_from_reader r =
  let buf = Bytes.create order_bytes in
  let%bind result = Reader.really_read r buf ~pos:0 ~len:order_bytes in
  match result with
  | `Eof n when n = 0 -> return None
  | `Eof n ->
      printf "Parial read (got %d bytes), closing connection\n%!" n;
      return None
  | `Ok ->
      let order = decode buf in
      return (Some order)

let accept_as_bytes_from_reader r =
  let buf = Bytes.create order_bytes in
  let%bind result = Reader.really_read r buf ~pos:0 ~len:order_bytes in
  match result with
  | `Eof n when n = 0 -> return None
  | `Eof n ->
      printf "Parial read (got %d bytes), closing connection\n%!" n;
      return None
  | `Ok -> return (Some buf)

let log_summary_from_bytes buf prefix =
  let id = Bytes.sub buf ~pos:0 ~len:16 |> Bytes.to_string |> String.strip in
  let price = EndianBytes.BigEndian.get_int32 buf 16 |> Int32.to_int_exn in
  let size = EndianBytes.BigEndian.get_int32 buf 20 |> Int32.to_int_exn in
  let side = Bytes.get buf 24 |> Char.to_int in
  let side_str = match side with 0 -> "Buy" | 1 -> "Sell" | _ -> "?" in
  let sequence_number = EndianBytes.BigEndian.get_int64 buf 36 in

  Log.Global.info "%sid=%s sequence_number=%s size=%d price=%d side=%s" prefix
    id
    (Int64.to_string sequence_number)
    size price side_str
