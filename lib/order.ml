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
  Bytes.blit id_bytes 0 buf 0 (min 16 (Bytes.length id_bytes));

  (* price and size *)
  EndianBytes.BigEndian.set_int32 buf 16 (Int32.of_int o.price);
  EndianBytes.BigEndian.set_int32 buf 20 (Int32.of_int o.size);

  (* side *)
  Bytes.set buf 24 (Char.chr (side_to_byte o.side));

  (* 3 bytes padding: already \000 *)

  (* timestamp *)
  EndianBytes.BigEndian.set_int64 buf 28 o.timestamp;

  (* sequence number *)
  EndianBytes.BigEndian.set_int64 buf 36 o.sequence_number;

  buf

let decode (buf : bytes) : t =
  if Bytes.length buf <> 44 then failwith "Invalid buffer length";

  let id = Bytes.sub_string buf 0 16 |> String.trim in
  let price = EndianBytes.BigEndian.get_int32 buf 16 |> Int32.to_int in
  let size = EndianBytes.BigEndian.get_int32 buf 20 |> Int32.to_int in
  let side = Bytes.get buf 24 |> Char.code |> byte_to_side in
  let timestamp = EndianBytes.BigEndian.get_int64 buf 28 in
  let sequence_number = EndianBytes.BigEndian.get_int64 buf 36 in

  { id; price; size; side; timestamp; sequence_number }
