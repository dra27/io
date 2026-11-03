open Base
open Ox_kernel
open Await_kernel
open Io_types
open Io_kernel
open Common_intf

module Prim = struct
  module type S = sig
    type t

    val close : #(Io.t * t) @ local -> unit#

    val read_or_cancel_exn
      :  #(Io.t * Cancellation.t * t * Io_slice.t) @ local
      -> Io.Expert.Io_result.t

    val is_nonblocking : #(Io.t * t) @ local -> bool
    val read_nonblocking : #(Io.t * t * Io_slice.t) @ local -> Io.Expert.Io_result.t
    val wait_or_cancel_exn : #(Io.t * Cancellation.t * t) @ local -> unit
  end

  type 's t = (module S with type t = 's)
end

module type Readable = sig @@ portable
  (** Abstract readable stream of bytes for input. *)

  (** {1 Interface} *)

  (** Represents a readable stream of bytes for input. *)
  type t : value mod many non_float unyielding

  (** {2 Blocking operations} *)

  (** [read io t buffer] blocks until something can be read or the stream is closed (by
      the writer) or end-of-stream has been reached, reads up to the whole [buffer], and
      returns the number of bytes read, which may be less than the length of the buffer.

      @raise Invalid_argument in case the buffer is empty.
      @raise Await_kernel.Await.Terminated
        in case [io] was terminated before any data transfer took place. *)
  val%template read
    :  Io.t @ local
    -> t @ local
    -> Io_slice.t @ local
    -> (isize Or_eof.t[@kind word]) @ l
  [@@mode l = (global, local)]

  include sig
    (** @closed *)

    (** [read_or_cancel io cancellation t buffer] is {{!read}
        [Completed (read io t buffer)]} unless the [cancellation] token was canceled
        before any data was read in which case the result is [Canceled].

        @raise Invalid_argument in case the buffer is empty.
        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before any data was read. *)
    val%template read_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local
      -> Io_slice.t @ local
      -> (isize Or_eof.t[@kind word]) Or_canceled.t @ l
    [@@mode l = (global, local)]
  end

  (** {2 Nonblocking operations}

      Support for nonblocking operations is optional and some IO mechanisms do not support
      nonblocking operations. *)

  (** [is_nonblocking io t] determines whether the readable supports nonblocking
      operations. *)
  val is_nonblocking : Io.t @ local -> t @ local -> bool

  (** [read_nonblocking io t buffer] is {{!read} [read io t buffer]} unless the operation
      would block in which case [Would_block] is returned immediately.

      @raise Invalid_argument
        in case the readable does not support nonblocking operations or the buffer is
        empty. *)
  val%template read_nonblocking
    :  Io.t @ local
    -> t @ local
    -> Io_slice.t @ local
    -> (isize Or_eof_or_would_block.t[@kind word]) @ l
  [@@mode l = (global, local)]

  (** {3 Readiness} *)

  (** [wait io r] suspends the current fiber until a {!read_nonblocking} on [r] should
      complete without blocking. Returns immediately if nonblocking operations are not
      supported.

      @raise Await_kernel.Await.Terminated
        in case [io] was terminated before the wait was satisfied. *)
  val wait : Io.t @ local -> t @ local -> unit

  include sig
    (** @closed *)

    (** [wait_or_cancel io cancellation t] is {{!wait} [Completed (wait io t)]} unless the
        [cancellation] token was canceled before the wait was satisfied in which case the
        result is [Canceled].

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before the wait was satisfied. *)
    val%template wait_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local
      -> unit Or_canceled.t @ l
    [@@mode l = (global, local)]
  end

  (** {1 Resource interface} *)

  include Resource with type t := t (** @inline *)

  (** {1 Expert interface for implementations} *)

  include sig
    (** @closed *)

    (** These interfaces are intended for implementations only. *)

    module Prim = Prim (** @inline *)

    module Expert : sig
      val%template create : 's @ p -> 's Prim.t @ p -> t @ p unique
      [@@mode p = (nonportable, portable)]
    end
  end
end
