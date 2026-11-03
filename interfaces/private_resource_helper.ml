open Ox_kernel
open Io_kernel
open Await_kernel
open Base

let%template[@inline] use ~close io (t @ local unique) ~f =
  match f (borrow_ t) with
  | value ->
    Vnit.box (close (Io.with_terminator io Terminator.unkillable) t);
    value
  | exception exn ->
    let bt = Backtrace.Exn.most_recent () in
    Vnit.box (close (Io.with_terminator io Terminator.unkillable) t);
    Exn.raise_with_original_backtrace exn bt
[@@kind k = (value, value & value, (value & value) & value)]
[@@mode p = (nonportable, portable)]
;;
