open Await_kernel
include Listener_intf

type t =
  | T :
      ('s : value mod contended).
      { state : 's @@ aliased many unyielding
      ; prim : 's Prim.t @@ aliased contended many unyielding
      }
      -> t

let close io (T { state; prim = (module Prim) }) = Prim.close #(io, state)

let%template use io t ~f = (Private_resource_helper.use [@mode p]) ~close io t ~f
[@@mode p = (nonportable, portable)]
;;

let accept_owned io t ~opts =
  let (T { state; prim = (module Prim) }) = t in
  Prim.accept_or_cancel_exn #(io, Cancellation.never, state, ~opts)
;;

let%template accept_owned_or_cancel io cancellation t ~opts =
  (Or_canceled.Exn.catch
  [@kind (value_or_null & value_or_null) & value_or_null] [@mode unique])
    (fun _ ->
       let (T { state; prim = (module Prim) }) = t in
       Prim.accept_or_cancel_exn #(io, cancellation, state, ~opts))
  [@nontail]
;;

let accept io t ~opts ~f =
  let #(~duplex, ~client_addr) = accept_owned io t ~opts in
  Duplex.use io (Duplex.Owned.unwrap duplex) ~f:(fun duplex ->
    f #(~duplex, ~client_addr) [@nontail])
  [@nontail]
;;

let addr io (T { state; prim = (module Prim) }) = Prim.addr #(io, state)

module Expert = struct
  let%template create state prim = T { state; prim } [@@mode p = (nonportable, portable)]
end
