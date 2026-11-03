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

    (** Allows zero length operations. *)
    val recv_or_cancel_exn
      :  #(Io.t * Cancellation.t * t * Io_slice.t) @ local
      -> #(len:isize * sender_addr:Sockaddr.t)

    (** Allows zero length operations. *)
    val recv_nonblocking
      :  #(Io.t * t * Io_slice.t) @ local
      -> #(len:Io.Expert.Io_result.t * sender_addr:Sockaddr.t or_null)

    (** Allows zero length operations. *)
    val send_or_cancel_exn
      :  #(Io.t * Cancellation.t * t * Io_slice.read_only * Sockaddr.t) @ local
      -> isize

    (** Allows zero length operations. *)
    val send_nonblocking
      :  #(Io.t * t * Io_slice.read_only * Sockaddr.t) @ local
      -> Io.Expert.Io_result.t

    val wait_or_cancel_exn
      :  #(Io.t * Cancellation.t * t * Io_wait_flags.t) @ local
      -> Io_wait_flags.t

    val addr : #(Io.t * t) @ local -> Sockaddr.t
  end

  type 's t = (module S with type t = 's)
end

module type Datagram_socket = sig @@ portable
  (** Abstract connectionless datagram socket. *)

  (** {1 Interface} *)

  (** Represents a socket configured for connectionless messaging. *)
  type t : value mod contended many non_float unyielding

  (** {2 Blocking operations} *)

  (** [send io t buffer addr] sends at most the entire buffer of data to the specified
      address and returns the number of bytes sent.

      Allows empty messages.

      @raise Await_kernel.Await.Terminated
        in case [io] was terminated before any data was sent. *)
  val send
    :  Io.t @ local
    -> t @ local
    -> Io_slice.t @ local read
    -> Sockaddr.t @ local
    -> isize

  include sig
    (** @closed *)

    (** [send_or_cancel io cancellation t buffer addr] is {{!send}
        [Completed (send io t buffer addr)]} unless the [cancellation] token was canceled
        before any data was sent in which case the result is [Canceled].

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before any data was sent. *)
    val%template send_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local
      -> Io_slice.t @ local read
      -> Sockaddr.t @ local
      -> (isize Or_canceled.t[@kind word]) @ l
    [@@mode l = (global, local)]
  end

  (** [recv io t buffer] receives at most the entire buffer of data sent to the socket and
      returns the length and address of the sender.

      Allows empty messages.

      @raise Await_kernel.Await.Terminated
        in case [io] was terminated before any data was received. *)
  val recv
    :  Io.t @ local
    -> t @ local
    -> Io_slice.t @ local
    -> #(len:isize * sender_addr:Sockaddr.t)

  include sig
    (** @closed *)

    (** [recv_or_cancel io cancellation t buffer addr] is {{!recv}
        [Completed (recv io t buffer addr)]} unless the [cancellation] token was canceled
        before any data was received in which case the result is [Canceled].

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before any data was received. *)
    val recv_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local
      -> Io_slice.t @ local
      -> (#(len:isize * sender_addr:Sockaddr.t) Or_canceled.t[@kind word & value_or_null])
  end

  (** {2 Nonblocking operations}

      Sockets always support nonblocking operations. *)

  (** [send_nonblocking io t buffer addr] is {{!send} [send io t buffer addr]} unless the
      operation would block in which case [Would_block] is returned immediately. *)
  val%template send_nonblocking
    :  Io.t @ local
    -> t @ local
    -> Io_slice.t @ local read
    -> Sockaddr.t @ local
    -> (isize Or_would_block.t[@kind word]) @ l
  [@@mode l = (global, local)]

  (** [recv_nonblocking io t buffer addr] is essentially {{!recv} [recv io t buffer addr]}
      unless the operation would block in which case [Would_block] is returned
      immediately. *)
  val recv_nonblocking
    :  Io.t @ local
    -> t @ local
    -> Io_slice.t @ local
    -> (#(len:isize * sender_addr:Sockaddr.t) Or_eof_or_would_block.t[@kind word & value])

  (** {3 Readiness} *)

  (** [wait_send io t] is {{!wait} [ignore (wait io t Io_wait_flags.out)]}.

      @raise Await_kernel.Await.Terminated
        in case [io] was terminated before the wait was satisfied. *)
  val wait_send : Io.t @ local -> t @ local -> unit

  (** [wait_recv io t] is {{!wait} [ignore (wait io t Io_wait_flags.in_)]}.

      @raise Await_kernel.Await.Terminated
        in case [io] was terminated before the wait was satisfied. *)
  val wait_recv : Io.t @ local -> t @ local -> unit

  include sig
    (** @closed *)

    (** [wait io t flags] suspends the current fiber until a {!recv_nonblocking} or
        {!send_nonblocking}, when [flags] contains {{!Io_types.Io_wait_flags.in_} [in_]}
        or {{!Io_types.Io_wait_flags.out} [out]} respectively, on [t] should complete
        without blocking. Returns flags corresponding to the operation or operations that
        should complete without blocking.

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before the wait was satisfied. *)
    val wait : Io.t @ local -> t @ local -> Io_wait_flags.t -> Io_wait_flags.t

    (** [wait_send_or_cancel io cancellation t] is {{!wait_send}
        [Completed (wait_send io t)]} unless the [cancellation] token was canceled before
        the wait was satisfied in which case the result is [Canceled].

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before the wait was satisfied. *)
    val wait_send_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local
      -> unit Or_canceled.t

    (** [wait_recv_or_cancel io cancellation t] is {{!wait_recv}
        [Completed (wait_recv io t)]} unless the [cancellation] token was canceled before
        the wait was satisfied in which case the result is [Canceled].

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before the wait was satisfied. *)
    val wait_recv_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local
      -> unit Or_canceled.t

    (** [wait_or_cancel io cancellation t flags] is {{!wait}
        [Completed (wait io t flags)]} unless the [cancellation] token was canceled before
        the wait was satisfied in which case the result is [Canceled].

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before the wait was satisfied. *)
    val%template wait_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local
      -> Io_wait_flags.t @ local
      -> Io_wait_flags.t Or_canceled.t @ l
    [@@mode l = (global, local)]
  end

  (** {2 Properties} *)

  (** [addr io t] is the address that the socket is bound to. *)
  val addr : Io.t @ local -> t @ local -> Sockaddr.t

  (** {1 Resource interface for advanced uses} *)

  include Resource with type t := t (** @inline *)

  (** {1 Expert interface for implementations} *)

  include sig
    (** @closed *)

    (** These interfaces are intended for implementations only. *)

    module Prim = Prim (** @inline *)

    module Expert : sig
      val%template create
        : ('s : value mod contended).
        's @ p -> 's Prim.t @ p -> t @ p unique
      [@@mode p = (nonportable, portable)]
    end
  end
end
