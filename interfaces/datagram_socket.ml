open Core
open Await_kernel
open Io_types
open Io_kernel
include Datagram_socket_intf

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

let recv_or_cancel_exn io cancellation (T { state; prim = (module Prim) }) buffer =
  (* Allows empty messages. *)
  Prim.recv_or_cancel_exn #(io, cancellation, state, buffer)
;;

let recv io t buffer = recv_or_cancel_exn io Cancellation.never t buffer

let recv_or_cancel io cancellation t buffer =
  (Or_canceled.Exn.catch [@kind word & value_or_null]) (fun _ ->
    recv_or_cancel_exn io cancellation t buffer)
  [@nontail]
;;

let recv_nonblocking io (T { state; prim = (module Prim) }) buffer =
  Io_slice.assert_non_empty buffer;
  let #(~len, ~sender_addr) = Prim.recv_nonblocking #(io, state, buffer) in
  match (Io.Expert.Io_result.to_or_eof_or_would_block [@mode local]) len with
  | Would_block -> (Would_block : (_ Or_eof_or_would_block.t[@kind word & value]))
  | Eof -> Eof
  | Ok len -> Ok #(~len, ~sender_addr:(Or_null.unsafe_value sender_addr))
;;

let send_or_cancel_exn io cancellation (T { state; prim = (module Prim) }) buffer dst =
  (* Allows empty messages. *)
  Prim.send_or_cancel_exn #(io, cancellation, state, { modal = buffer }, dst)
;;

let send io t buffer dst = send_or_cancel_exn io Cancellation.never t buffer dst

let%template send_or_cancel io cancellation t buffer dst =
  (Or_canceled.Exn.catch [@mode l] [@kind word]) (fun _ ->
    send_or_cancel_exn io cancellation t buffer dst)
  [@nontail] [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

let%template send_nonblocking io (T { state; prim = (module Prim) }) buffer dst =
  (* Allows empty messages. *)
  let len = Prim.send_nonblocking #(io, state, { modal = buffer }, dst) in
  (Io.Expert.Io_result.to_or_would_block [@mode l])
    len [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

let wait_or_cancel_exn io cancellation (T { state; prim = (module Prim) }) flags =
  Prim.wait_or_cancel_exn #(io, cancellation, state, flags)
;;

let wait io t flags = wait_or_cancel_exn io Cancellation.never t flags
let wait_send io t = ignore (wait io t Io_wait_flags.out : Io_wait_flags.t)
let wait_recv io t = ignore (wait io t Io_wait_flags.in_ : Io_wait_flags.t)

let%template wait_or_cancel io cancellation t flags =
  (Or_canceled.Exn.catch [@mode l]) (fun _ -> wait_or_cancel_exn io cancellation t flags)
  [@nontail] [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

let wait_send_or_cancel io cancellation t =
  Or_canceled.Exn.catch (fun _ ->
    ignore (wait_or_cancel_exn io cancellation t Io_wait_flags.out : Io_wait_flags.t))
  [@nontail]
;;

let wait_recv_or_cancel io cancellation t =
  Or_canceled.Exn.catch (fun _ ->
    ignore (wait_or_cancel_exn io cancellation t Io_wait_flags.in_ : Io_wait_flags.t))
  [@nontail]
;;

let addr io (T { state; prim = (module Prim) }) = Prim.addr #(io, state)

module Expert = struct
  let%template create state prim = T { state; prim } [@@mode p = (nonportable, portable)]
end
