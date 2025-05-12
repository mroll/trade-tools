open Core
open Async

type session = {
  id : Uuid.t;
  port : int;
  client_addr : Socket.Address.Inet.t;
  started_at : Time_float.t;
}

type t = {
  port_range : int * int;
  used_ports : Int.Hash_set.t;
  sessions : (Uuid.t, session) Hashtbl.t;
  lock : unit Mvar.Read_write.t;
}

module Uuid_key = struct
  type t = Uuid.t [@@deriving sexp_of]

  let hash = Hashtbl.hash
  let compare = Uuid.compare
end

let rng = Random.State.make_self_init ()
let gen_uuid () = Uuid.create_random rng

let create (low, high) =
  let lock = Mvar.create () in
  let%map () = Mvar.put lock () in
  {
    port_range = (low, high);
    used_ports = Int.Hash_set.create ();
    sessions = Hashtbl.create (module Uuid_key);
    lock;
  }

let active_sessions t = Hashtbl.data t.sessions

let allocate_port t =
  let low, high = t.port_range in
  let rec find p =
    if p > high then None
    else if Hash_set.mem t.used_ports p then find (p + 1)
    else Some p
  in
  find low

let release_port t port = Hash_set.remove t.used_ports port

let shutdown_session t session =
  release_port t session.port;
  Hashtbl.remove t.sessions session.id;
  Deferred.unit

let handle_server_handshake t f addr _r w =
  let open Deferred.Let_syntax in
  let%bind () = Mvar.take t.lock in
  match allocate_port t with
  | None ->
      let%bind () =
        Writer.write_line w "ERROR: No ports available";
        Writer.flushed w
      in
      let%bind () = Mvar.put t.lock () in
      return ()
  | Some port ->
      Hash_set.add t.used_ports port;
      let%bind () = Mvar.put t.lock () in

      let session_id = gen_uuid () in
      let session =
        {
          id = session_id;
          port;
          client_addr = addr;
          started_at = Time_float.now ();
        }
      in
      Hashtbl.set t.sessions ~key:session_id ~data:session;

      let%bind _server =
        Tcp.Server.create ~on_handler_error:`Raise
          (Tcp.Where_to_listen.of_port port)
          f
      in

      let%bind () =
        Writer.write_line w (Int.to_string port);
        Writer.flushed w
      in
      return ()

let handle_client_handshake host port =
  let%bind socket =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port (Host_and_port.create ~host ~port))
  in
  let _sock, reader, _writer = socket in
  let%bind result = Reader.read_line reader in
  match result with
  | `Ok line -> (
      match Int.of_string_opt (String.strip line) with
      | Some port -> return port
      | None -> failwithf "Invalid session port received: %s" line ())
  | `Eof -> failwith "Unexpected EOF while reading session port"
