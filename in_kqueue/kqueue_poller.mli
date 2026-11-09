@@ portable

open Await

(** A [kqueue] and a poller thread for awaiting readiness of file descriptors.

    Every wait registers its own one-shot knote (using Darwin's [EV_UDATA_SPECIFIC]), so
    any number of fibers may wait on the same descriptor concurrently and each wait can be
    cancelled individually. *)

type t : value mod contended portable unyielding

(** [create ()] creates a new kqueue and spawns a poller thread on the current domain. *)
val create : unit -> t

(** [close t] stops the poller thread and closes the kqueue. [t] must not be used
    afterwards and there must be no pending waits. *)
val close : t -> unit

module Filters : sig
  (** Set of conditions to wait for. *)
  type t = int

  val in_ : t
  val out : t
  val in_out : t

  (** [err] is only ever included in results, never in requests. It indicates an error or
      end-of-file condition on the descriptor. *)
  val err : t

  val mem : t -> t -> bool
end

(** [wait_or_cancel_exn await cancel t fd filters] suspends the current fiber until [fd]
    is ready for at least one of the [filters] and returns the ready filters.

    The result may be spurious, i.e. the caller must be prepared to find that the
    descriptor is not actually ready.

    @raise Await.Terminated in case [await] was terminated during the wait.
    @raise Or_canceled.Exn.Canceled in case [cancel] was canceled during the wait.
    @raise Unix.Unix_error in case the wait could not be registered. *)
val wait_or_cancel_exn
  :  Await.t @ local
  -> Cancellation.t @ local
  -> t @ local
  -> Core_unix.File_descr.t
  -> Filters.t
  -> Filters.t

(** [poll fd filters] is a non-blocking readiness check of [fd] for [filters]. Returns the
    ready filters, which may be empty.

    Regular files are always reported as ready. *)
val poll : Core_unix.File_descr.t -> Filters.t -> Filters.t

(** {1 Low-level system call wrappers}

    These return a non-negative result or a negated errno. *)

module Syscall : sig
  val eagain : int
  val eintr : int
  val einprogress : int
  val eexist : int
  val enoent : int
  val eisdir : int

  (** [raise_unix_error errno ~name] raises [Unix.Unix_error] for [errno]. *)
  val raise_unix_error : int -> name:string -> 'a

  val set_nosigpipe : Core_unix.File_descr.t -> int
  val set_nonblock : Core_unix.File_descr.t -> int
end

module For_testing : sig
  (** [appears_idle t] is [true] if there are no registered waiters. Only reliable when
      [t] is not used concurrently. *)
  val appears_idle : t -> bool
end
