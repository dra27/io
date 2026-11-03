open Await_kernel
open Io_types
open Io_kernel
include Readable_intf

type t =
  | T :
      { state : 's @@ aliased many unyielding
      ; prim : 's Prim.t @@ aliased contended many unyielding
      }
      -> t

let close io (T { state; prim = (module Prim) }) = Prim.close #(io, state)

let%template use io t ~f = (Private_resource_helper.use [@mode p]) ~close io t ~f
[@@mode p = (nonportable, portable)]
;;

let read_or_cancel_exn io cancellation (T { state; prim = (module Prim) }) buffer =
  Io_slice.assert_non_empty buffer;
  Prim.read_or_cancel_exn #(io, cancellation, state, buffer)
;;

let%template read io t buffer : (_ Or_eof.t[@kind word]) =
  let len = read_or_cancel_exn io Cancellation.never t buffer in
  (Io.Expert.Io_result.to_or_eof [@mode l])
    len [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

let%template read_or_cancel io cancellation t buffer =
  (Or_canceled.Exn.catch [@mode l]) (fun _ : (_ Or_eof.t[@kind word]) ->
    let len = read_or_cancel_exn io cancellation t buffer in
    (Io.Expert.Io_result.to_or_eof [@mode l])
      len [@exclave_if_local l ~reasons:[ Will_return_unboxed ]])
  [@nontail] [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

let is_nonblocking io (T { state; prim = (module Prim) }) =
  Prim.is_nonblocking #(io, state)
;;

let%template read_nonblocking io (T { state; prim = (module Prim) }) buffer =
  Io_slice.assert_non_empty buffer;
  let len = Prim.read_nonblocking #(io, state, buffer) in
  (Io.Expert.Io_result.to_or_eof_or_would_block [@mode l])
    len [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

let wait_or_cancel_exn io cancellation (T { state; prim = (module Prim) }) =
  Prim.wait_or_cancel_exn #(io, cancellation, state)
;;

let wait io t = wait_or_cancel_exn io Cancellation.never t

let%template wait_or_cancel io cancellation t =
  (Or_canceled.Exn.catch [@mode l]) (fun _ -> wait_or_cancel_exn io cancellation t)
  [@nontail] [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

module Expert = struct
  let%template create state prim = T { state; prim } [@@mode p = (nonportable, portable)]
end
