open Core
open Async

type t = (Int64.t, read_write) Mvar.t

let create (initial : Int64.t) : t =
  let m = Mvar.create () in
  don't_wait_for (Mvar.put m initial);
  m

let next (seq : t) : Int64.t Deferred.t =
  let%bind current = Mvar.take seq in
  let next = Int64.(current + 1L) in
  let%bind () = Mvar.put seq next in
  return current
