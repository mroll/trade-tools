open Core
open Async

type session = {
  id : Uuid.t;
  port : int;
  client_addr : Socket.Address.Inet.t;
  started_at : Time_float.t;
}

type t

val create : int * int -> t Deferred.t
val active_sessions : t -> session list
val shutdown_session : t -> session -> unit Deferred.t

val handle_handshake :
  t ->
  (Socket.Address.Inet.t -> Reader.t -> Writer.t -> unit Deferred.t) ->
  Socket.Address.Inet.t ->
  Reader.t ->
  Writer.t ->
  unit Deferred.t
