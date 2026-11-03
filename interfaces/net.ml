open Await_kernel
include Net_intf

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

let addr_info io (T { state; prim = (module Prim) }) addr =
  Prim.addr_info #(io, state, addr)
;;

let stream_addrs io (T { state; prim = (module Prim) }) ~host ~service =
  Prim.stream_addrs #(io, state, ~host, ~service)
;;

let connect_owned io t ~opts addr =
  let (T { state; prim = (module Prim) }) = t in
  Prim.connect_or_cancel_exn #(io, Cancellation.never, state, ~opts, addr)
;;

let connect_owned_or_cancel io cancellation t ~opts addr =
  (Or_canceled.Exn.catch [@kind value_or_null & value_or_null] [@mode unique]) (fun _ ->
    let (T { state; prim = (module Prim) }) = t in
    Prim.connect_or_cancel_exn #(io, cancellation, state, ~opts, addr))
  [@nontail]
;;

let connect io t ~opts addr ~f =
  Duplex.use io (Duplex.Owned.unwrap (connect_owned io t ~opts addr)) ~f
;;

let%template listen_or_cancel_exn io cancellation t ~opts ~max_pending_requests addr =
  let (T { state; prim = (module Prim) }) = t in
  (Prim.listen_or_cancel_exn [@mode p])
    #(io, cancellation, state, ~opts, ~max_pending_requests, addr)
[@@mode p = (nonportable, portable)]
;;

let%template listen_unique io t ~opts ~max_pending_requests addr =
  (listen_or_cancel_exn [@mode p])
    io
    Cancellation.never
    t
    ~opts
    ~max_pending_requests
    addr
[@@mode p = (nonportable, portable)]
;;

let%template listen io t ~opts ~max_pending_requests addr ~f =
  (Listener.use [@mode p]) io ~f
  @@ (listen_unique [@mode p]) io t ~opts ~max_pending_requests addr
[@@mode p = (nonportable, portable)]
;;

let%template listen_unique_or_cancel io cancellation t ~opts ~max_pending_requests addr =
  (Or_canceled.Exn.catch [@mode unique p]) (fun _ ->
    (listen_or_cancel_exn [@mode p]) io cancellation t ~opts ~max_pending_requests addr)
  [@nontail]
[@@mode p = (nonportable, portable)]
;;

let datagram_addrs io (T { state; prim = (module Prim) }) ~host ~service =
  Prim.datagram_addrs #(io, state, ~host, ~service)
;;

let%template bind_datagram_socket_or_cancel_exn io cancellation t ~opts addr =
  let (T { state; prim = (module Prim) }) = t in
  (Prim.bind_datagram_socket_or_cancel_exn [@mode p])
    #(io, cancellation, state, ~opts, addr)
[@@mode p = (nonportable, portable)]
;;

let%template bind_datagram_socket_unique io t ~opts addr =
  (bind_datagram_socket_or_cancel_exn [@mode p]) io Cancellation.never t ~opts addr
[@@mode p = (nonportable, portable)]
;;

let%template bind_datagram_socket io t ~opts addr ~f =
  (Datagram_socket.use [@mode p]) io ~f
  @@ (bind_datagram_socket_unique [@mode p]) io t ~opts addr
[@@mode p = (nonportable, portable)]
;;

let%template bind_datagram_socket_unique_or_cancel io cancellation t ~opts addr =
  (Or_canceled.Exn.catch [@mode unique p]) (fun _ ->
    (bind_datagram_socket_or_cancel_exn [@mode p]) io cancellation t ~opts addr)
  [@nontail]
[@@mode p = (nonportable, portable)]
;;

module Expert = struct
  let%template create state prim = T { state; prim } [@@mode p = (nonportable, portable)]
end
