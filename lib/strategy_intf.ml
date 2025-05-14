open Async

module type STRATEGY = sig
  val init : unit -> unit
  val handle_event : L3_event.t -> Writer.t -> unit
end

let (strategy_impl : (module STRATEGY) option ref) = ref None
let register (impl : (module STRATEGY)) = strategy_impl := Some impl

let get () =
  match !strategy_impl with
  | Some m -> m
  | None -> failwith "Strategy not loaded"
