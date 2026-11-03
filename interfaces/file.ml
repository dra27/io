open Ox_kernel
open Await_kernel
include File_intf

type -'perms t =
  | T :
      'perms ('s : value mod contended).
      { state : 's @@ aliased many unyielding
      ; prim : 's Prim.t @@ aliased contended many unyielding
      }
      -> 'perms t

let close io (T { state; prim = (module Prim) }) = Prim.close #(io, state)

let%template use io t ~f = (Private_resource_helper.use [@mode p]) ~close io t ~f
[@@mode p = (nonportable, portable)]
;;

let size io (T { state; prim = (module Prim) }) = Prim.size #(io, state)

let resize io (T { state; prim = (module Prim) }) size =
  Vnit.box (Prim.resize #(io, state, size))
;;

let%template readable_unique io (T { state; prim = (module Prim) }) ~offset =
  (Prim.readable [@mode p]) #(io, state, ~offset)
[@@mode p = (nonportable, portable)]
;;

let%template readable io t ~offset ~f =
  (Readable.use [@mode p]) io ((readable_unique [@mode p]) io t ~offset) ~f
[@@mode p = (nonportable, portable)]
;;

let%template writable_unique io (T { state; prim = (module Prim) }) ~offset =
  (Prim.writable [@mode p]) #(io, state, ~offset)
[@@mode p = (nonportable, portable)]
;;

let%template writable io t ~offset ~f =
  (Writable.use [@mode p]) io ((writable_unique [@mode p]) io t ~offset) ~f
[@@mode p = (nonportable, portable)]
;;

let read_or_cancel_exn io cancellation t ~offset buf =
  let (T { state; prim = (module Prim) }) = t in
  Prim.read_or_cancel_exn #(io, cancellation, state, ~offset, buf)
;;

let read io t ~offset buf = read_or_cancel_exn io Cancellation.never t ~offset buf

let%template read_or_cancel io cancellation t ~offset buf =
  (Or_canceled.Exn.catch [@kind word] [@mode l]) (fun _ ->
    read_or_cancel_exn io cancellation t ~offset buf)
  [@nontail] [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

let write_or_cancel_exn io cancellation t ~offset buf =
  let (T { state; prim = (module Prim) }) = t in
  Prim.write_or_cancel_exn #(io, cancellation, state, ~offset, { modal = buf })
;;

let write io t ~offset buf = write_or_cancel_exn io Cancellation.never t ~offset buf

let%template write_or_cancel io cancellation t ~offset buf =
  (Or_canceled.Exn.catch [@kind word] [@mode l]) (fun _ ->
    write_or_cancel_exn io cancellation t ~offset buf)
  [@nontail] [@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
[@@mode l = (global, local)]
;;

let flush io (T { state; prim = (module Prim) }) = Vnit.box (Prim.flush #(io, state))

module Expert = struct
  let%template create state prim = T { state; prim } [@@mode p = (nonportable, portable)]
end
