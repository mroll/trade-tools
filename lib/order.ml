open Core
open Async

type t = Add of Add.t | Cancel of Cancel.t

let order_width = 49
let typ_width = 3

let accept_from_reader r : t option Deferred.t =
  let buf = Bytes.create typ_width in
  let%bind result = Reader.really_read r buf ~pos:0 ~len:typ_width in
  match result with
  | `Eof n when n = 0 -> return None
  | `Eof n ->
      printf "Partial read (got %d bytes), closing connection\n%!" n;
      return None
  | `Ok -> (
      let typ =
        Bytes.sub buf ~pos:0 ~len:typ_width |> Bytes.to_string |> String.strip
      in
      match typ with
      | "add" ->
          let%map add_opt = Add.accept_from_reader r in
          Option.map add_opt ~f:(fun add -> Add add)
      | "can" ->
          let%map cancel_opt = Cancel.accept_from_reader r in
          Option.map cancel_opt ~f:(fun cancel -> Cancel cancel)
      | _ ->
          Log.Global.info "Invalid order type: %s" typ;
          return None)

let accept_as_bytes_from_reader r =
  let buf = Bytes.create order_width in
  let%bind result = Reader.really_read r buf ~pos:0 ~len:order_width in
  match result with
  | `Eof n when n = 0 -> return None
  | `Eof n ->
      printf "Partial read (got %d bytes), closing connection\n%!" n;
      return None
  | `Ok -> return (Some buf)

let log_summary_from_bytes buf prefix =
  let typ =
    Bytes.sub buf ~pos:0 ~len:typ_width |> Bytes.to_string |> String.strip
  in
  match typ with
  | "add" -> Add.log_summary_from_bytes buf prefix
  | "can" -> Cancel.log_summary_from_bytes buf prefix
  | _ -> Log.Global.info "Invalid order type: %s" typ
