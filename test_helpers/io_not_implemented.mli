open Await_kernel
open Io_kernel

(** [create await] is an [Io.t] implementation whose operations fail with
    [Failure "Not implemented"]. This can be useful for testing IO interface
    implementation that do not use system level IO. *)
val create : Await.t @ local -> Io.t @ local
