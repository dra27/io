open Await_kernel
open Io_types
open Io_kernel
include Writable_intf

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

let write_or_cancel_exn io cancellation (T { state; prim = (module Prim) }) buffer =
  let ({ modal = buffer } : Io_slice.read_only) = buffer in
  Io_slice.assert_non_empty buffer;
  Prim.write_or_cancel_exn #(io, cancellation, state, { modal = buffer })
;;

let%template write io t buffer : (_ Or_eof.t[@kind word]) =
  let len = write_or_cancel_exn io Cancellation.never t { modal = buffer } in
  (Io.Expert.Io_result.to_or_eof [@mode l])
    len [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

let%template write_or_cancel io cancellation t buffer =
  (Or_canceled.Exn.catch [@mode l]) (fun _ : (_ Or_eof.t[@kind word]) ->
    let len = write_or_cancel_exn io cancellation t { modal = buffer } in
    (Io.Expert.Io_result.to_or_eof [@mode l])
      len [@exclave_if_local l ~reasons:[ Will_return_unboxed ]])
  [@nontail] [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

let is_nonblocking io (T { state; prim = (module Prim) }) =
  Prim.is_nonblocking #(io, state)
;;

let%template write_nonblocking io (T { state; prim = (module Prim) }) buffer =
  Io_slice.assert_non_empty buffer;
  let len = Prim.write_nonblocking #(io, state, { modal = buffer }) in
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
