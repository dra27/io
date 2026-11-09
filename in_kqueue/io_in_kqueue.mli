@@ portable

open Await
open Io_kernel

(** A [kqueue] based implementation of {!Io.t} for macOS.

    This provides the same interface as the [io_uring] based [Io_in_uring] on Linux, but
    is readiness based:

    - Stream and datagram operations first attempt the system call without blocking. Only
      if it would block is the fiber suspended until the descriptor is ready, using a
      one-shot knote per waiter in a [kqueue] serviced by a poller thread. Sockets created
      by this implementation are non-blocking and have [SO_NOSIGPIPE] set. Blocking
      descriptors, such as the standard streams, pipes and ttys, are also supported: their
      readiness is checked before the system call is made, so the calling thread is not
      blocked.

    - Operations on regular files ([pread], [pwrite], [fsync], ...) and on the file system
      ([openat], [mkdirat], [fstatat], ...) are performed directly on the calling thread
      with the runtime lock released, i.e. they complete synchronously rather than
      suspending the fiber. This is usually fastest for local file systems, but means a
      slow file system blocks the calling thread for the duration of the operation.

    The poller thread runs on the domain that called {!create}, so for the lowest wake-up
    latency that domain should not be continuously busy running OCaml code.

    As with [Io_in_uring], closing a descriptor while another fiber is waiting on it
    leaves that wait pending (until canceled or terminated).

    See {!Kqueue_poller} for details of how waiting and cancellation work. *)

module Kqueue_poller = Kqueue_poller

(** Represents a [kqueue] and its poller thread. *)
type t = Kqueue_poller.t

(** [create ()] creates a new [kqueue] and spawns a poller thread for it on the current
    domain. A single [t] can be shared by any number of fibers on any number of domains. *)
val create : unit -> t

(** [close t] stops the poller thread and closes the [kqueue]. There must be no pending
    operations using [t] and [t] must not be used afterwards. *)
val close : t -> unit

(** [with_io await t ~f] calls [f] with an [Io.t] implemented using [t]. *)
val with_io
  : ('r : value_or_null).
  Await.t @ local
  -> t @ local
  -> f:(Io.t @ local -> 'r @ once unique) @ local once
  -> 'r @ once unique
