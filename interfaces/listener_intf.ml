open Base
open Await_kernel
open Io_types
open Io_kernel
open Common_intf

module Prim = struct
  module type S = sig
    type t

    val close : #(Io.t * t) @ local -> unit#

    val accept_or_cancel_exn
      :  #(Io.t * Cancellation.t * t * opts:Socket_opts.t) @ local
      -> #(duplex:Duplex.Owned.t# * client_addr:Sockaddr.t) @ unique

    val addr : #(Io.t * t) @ local -> Sockaddr.t
  end

  type 's t = (module S with type t = 's)
end

module type Listener = sig @@ portable
  (** Abstract listener for point-to-point stream connections. *)

  (** {1 Interface} *)

  (** Represents a socket configured for listening for connections. *)
  type t : value mod contended many non_float unyielding

  (** {2 Blocking operations} *)

  (** [accept io t ~opts ~f] is
      {[
        let #(~duplex, ~client_addr) = accept_owned io t ~opts in
        let%with.tilde.stack duplex = Duplex.use io duplex in
        f #(~duplex, ~client_addr)
      ]}
      which ensures that no resources are leaked. *)
  val accept
    : ('r : value_or_null).
    Io.t @ local
    -> t @ local
    -> opts:Socket_opts.t @ local
    -> f:(#(duplex:Duplex.t# * client_addr:Sockaddr.t) @ local -> 'r @ once unique)
       @ local once
    -> 'r @ once unique

  include sig
    (** @closed *)

    (** [accept_owned io t ~client_opts] waits until a client connects and then returns
        the readable and writable streams and the address of the connected client.

        The socket [opts] are used to configure the socket underlying the returned
        streams.

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before a client connected. *)
    val accept_owned
      :  Io.t @ local
      -> t @ local
      -> opts:Socket_opts.t @ local
      -> #(duplex:Duplex.Owned.t# * client_addr:Sockaddr.t) @ unique

    (** [accept_owned_or_cancel io cancellation t ~opts] is {{!accept_owned}
        [Completed (accept_owned io t ~opts)]} unless the [cancellation] token was
        canceled before a client connected in which case the result is [Canceled].

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before a client connected. *)
    val accept_owned_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local
      -> opts:Socket_opts.t @ local
      -> (#(duplex:Duplex.Owned.t# * client_addr:Sockaddr.t) Or_canceled.t
         [@kind (value_or_null & value_or_null) & value_or_null])
         @ unique
  end

  (** {2 Properties} *)

  (** [addr io t] is the address that the listener is bound to. *)
  val addr : Io.t @ local -> t @ local -> Sockaddr.t

  (** {1 Resource interface} *)

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
