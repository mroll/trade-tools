open Async
open Core
open Trade_lib

let () =
  Strategy_intf.register
    (module struct
      let init _w _oracle_host _oracle_port =
        print_endline "Init";
        return ()

      let handle_event ev _w =
        Log.Global.info "Got event: %s"
          (Yojson.Safe.to_string (L3_event.yojson_of_l3_event ev))
    end)
