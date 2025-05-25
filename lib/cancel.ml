open Core
open Async

type t = {
  id : string;
  typ : string;
  timestamp : int64;
  sequence_number : int64;
}

let width = 35

let decode (buf : bytes) : t =
  if Bytes.length buf <> width then failwith "Invalid buffer length";

  let id = Bytes.sub buf ~pos:0 ~len:16 |> Bytes.to_string |> String.strip in
  let timestamp = EndianBytes.BigEndian.get_int64 buf 28 in
  let sequence_number = EndianBytes.BigEndian.get_int64 buf 36 in

  { id; typ = "add"; timestamp; sequence_number }

let accept_from_reader r =
  let buf = Bytes.create width in
  let%bind result = Reader.really_read r buf ~pos:0 ~len:width in
  match result with
  | `Eof n when n = 0 -> return None
  | `Eof n ->
      printf "Partial read (got %d bytes), closing connection\n%!" n;
      return None
  | `Ok ->
      let add = decode buf in
      return (Some add)

let log_summary_from_bytes buf prefix =
  let id = Bytes.sub buf ~pos:0 ~len:16 |> Bytes.to_string |> String.strip in
  let sequence_number = EndianBytes.BigEndian.get_int64 buf 36 in

  Log.Global.info "%sid=%s type=cancel sequence_number=%s" prefix id
    (Int64.to_string sequence_number)
