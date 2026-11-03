@@ portable

open Await
open Io_uring_bindings

(** A handle to a sharded set of [io_uring] rings, one per domain, sharing a
    configuration, and a call interface to submit operations and await their completion
    with cancellation. *)

module Config : sig
  (** Represents a configuration for a set of [io_uring] rings. *)
  type t : immutable_data

  (** Default [io_uring] configuration. *)
  val default : t
end

(** Represents a handle to a set of [io_uring] rings sharing a configuration. *)
type t : value mod contended portable unyielding

(** [create config] creates a new handle to a set of [io_uring] rings. *)
val create : Config.t -> t
[@@alert
  experimental
    "Io_uring_shards should currently only be used internally by the Io_in_uring \
     library.  The interface of Io_uring_shards is likely to go through some substantial \
     changes soon to support additional ways to use io_uring."]

(** [pinned await t ~f] configures the handle [t] to a set of [io_uring] rings to not
    destroy any rings during the call of [f].

    Without pinning, rings are added to the set as needed and destroyed once no longer
    needed. *)
val pinned
  : ('r : value_or_null).
  Await.t @ local
  -> t @ local
  -> f:(unit# -> 'r @ once unique) @ local once
  -> 'r @ once unique

(** In case the given result was [Error ECANCELED], raises {{!Await.Terminated}
    [Terminated]} if [await] was terminated and {{!Or_canceled.Exn.Canceled} [Canceled]}
    if not. Otherwise raises {{!Core_unix.Unix_error}
    [Unix_error (error_exn result, name, "")]}. *)
val raise_terminated_or_canceled_or_error
  : ('r : value_or_null).
  Await.t @ local -> Result.t -> name:string -> 'r @ portable unique

(** Does nothing if the given result isn't an error. Otherwise acts like
    {!raise_terminated_or_canceled_or_error}. *)
val on_error_raise_terminated_or_canceled_or_error
  :  Await.t @ local
  -> Result.t
  -> name:string
  -> unit

(** [call_sq_or_cancel await cancel t ~n ~f] calls [f] to submit [n] requests, waits for
    the completions, and returns a most specific completion result.

    More specifically, [call_sq_or_cancel await cancel t ~n ~f] ensures that the
    submission queue has space for [n] requests, calls [f #(sq, tag)] to push requests,
    submits any requests pushed by [f], sets up cancellation such that all of the requests
    with the [tag] are cancelled in case [await] is terminated or [cancel] is cancelled,
    and finally waits for [n] completions with the [tag] to arrive.

    This means that [f] should push requests to generate exactly [n] completions with the
    [tag] and the callback [f] should not raise.

    To determine a most specific result to return, [Ok 0] is considered the least specific
    result, i.e. less specific than any other [Ok _] or [Error _], any [Error _] is
    considered more specific than any [Ok _], and [Error ECANCELED] is considered the
    least specific error. Otherwise results are unordered. *)
val call_sq_or_cancel
  :  Await.t @ local
  -> Cancellation.t @ local
  -> t @ local
  -> n:int
  -> f:(#(Sq.t Capsule.Scoped.t * Tag.t) @ local -> unit) @ local once unyielding
  -> Result.t

(** [call_sq await t ~n ~f] is [call_sq_or_cancel await Cancellation.never t ~n ~f]. *)
val call_sq
  :  Await.t @ local
  -> t @ local
  -> n:int
  -> f:(#(Sq.t Capsule.Scoped.t * Tag.t) @ local -> unit) @ local once unyielding
  -> Result.t

(** [call_sq_0_or_cancel_exn await cancel t ~n ~name ~f] is
    [call_sq_or_cancel await cancel t ~n ~f] followed by
    [on_error_raise_terminated_or_canceled_or_error await result ~name].

    @raise Await.Terminated
      in case the result was [Error ECANCELED] and [await] was terminated.

    @raise Or_canceled.Exn.Canceled
      in case the result was [Error ECANCELED] and [await] was not terminated.

    @raise Core_unix.Unix_error
      in case the result was [Error _], but not [Error ECANCELED]. *)
val call_sq_0_or_cancel_exn
  :  Await.t @ local
  -> Cancellation.t @ local
  -> t @ local
  -> n:int
  -> name:string
  -> f:(#(Sq.t Capsule.Scoped.t * Tag.t) @ local -> unit) @ local once unyielding
  -> unit

(** [call_sq_0_exn await t ~n ~name ~f] is
    [call_sq_0_or_cancel_exn await Cancellation.never t ~n ~name ~f]. *)
val call_sq_0_exn
  :  Await.t @ local
  -> t @ local
  -> n:int
  -> name:string
  -> f:(#(Sq.t Capsule.Scoped.t * Tag.t) @ local -> unit) @ local once unyielding
  -> unit

(** [call_sq_non_neg_or_cancel_exn await cancel t ~n ~name ~f] is
    [call_sq_or_cancel await cancel t ~n ~f] followed by
    [raise_terminated_or_canceled_or_error await result ~name] in case the result was an
    error and otherwise returns the integer value of the result.

    @raise Await.Terminated
      in case the result was [Error ECANCELED] and [await] was terminated.

    @raise Or_canceled.Exn.Canceled
      in case the result was [Error ECANCELED] and [await] was not terminated.

    @raise Core_unix.Unix_error
      in case the result was [Error _], but not [Error ECANCELED]. *)
val call_sq_non_neg_or_cancel_exn
  :  Await.t @ local
  -> Cancellation.t @ local
  -> t @ local
  -> n:int
  -> name:string
  -> f:(#(Sq.t Capsule.Scoped.t * Tag.t) @ local -> unit) @ local once unyielding
  -> int

(** [call_sq_fd_or_cancel_exn await cancel t ~n ~name ~f] is
    [call_sq_non_neg_or_cancel_exn await cancel t ~n ~name ~f] except the result is
    returned as a file descriptor.

    @raise Await.Terminated
      in case the result was [Error ECANCELED] and [await] was terminated.

    @raise Or_canceled.Exn.Canceled
      in case the result was [Error ECANCELED] and [await] was not terminated.

    @raise Core_unix.Unix_error
      in case the result was [Error _], but not [Error ECANCELED]. *)
val call_sq_fd_or_cancel_exn
  :  Await.t @ local
  -> Cancellation.t @ local
  -> t @ local
  -> n:int
  -> name:string
  -> f:(#(Sq.t Capsule.Scoped.t * Tag.t) @ local -> unit) @ local once unyielding
  -> Caml_unix.file_descr

(** [call_sq_fd_exn await t ~n ~name ~f] is
    [call_sq_fd_or_cancel_exn await Cancellation.never t ~n ~name ~f]. *)
val call_sq_fd_exn
  :  Await.t @ local
  -> t @ local
  -> n:int
  -> name:string
  -> f:(#(Sq.t Capsule.Scoped.t * Tag.t) @ local -> unit) @ local once unyielding
  -> Caml_unix.file_descr

module For_testing : sig
  val appears_idle : t @ local -> bool
end
