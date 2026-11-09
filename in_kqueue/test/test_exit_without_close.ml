(* The poller thread must not keep the process alive: a program that never calls
   [Io_in_kqueue.close] must still exit. *)

open Core
open Await
open Io_kernel

let () =
  let poller = Io_in_kqueue.create () in
  let r, w = Core_unix.pipe () in
  ignore (Core_unix.write_substring w ~buf:"x" : int);
  let await =
    (Await.Expert.create [@alloc stack])
      ~sync:Sync.blocking
      ~terminator:Terminator.unkillable
  in
  let n =
    Io_in_kqueue.with_io await poller ~f:(fun io ->
      let fd = Io_descr_native.(create Roles.readable) ~shutdown:false r in
      let flags =
        Io.Expert.wait_or_cancel_exn #(io, Cancellation.never, fd, Io_types.Io_wait_flags.in_)
      in
      Io_types.Io_wait_flags.(do_intersect in_) flags)
  in
  assert n;
  print_endline "ok: exiting without closing the poller"
;;
