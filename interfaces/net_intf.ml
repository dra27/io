open Base
open Await_kernel
open Io_types
open Io_kernel
open Common_intf

module Prim = struct
  module type S = sig
    type t

    val close : #(Io.t * t) @ local -> unit#
    val addr_info : #(Io.t * t * Sockaddr.t) @ local -> #(host:string * service:string)

    val stream_addrs
      :  #(Io.t * t * host:string * service:string) @ local
      -> Sockaddr.t list

    val connect_or_cancel_exn
      :  #(Io.t * Cancellation.t * t * opts:Socket_opts.t * Sockaddr.t) @ local
      -> Duplex.Owned.t# @ unique

    val%template listen_or_cancel_exn
      :  #(Io.t
          * Cancellation.t
          * t
          * opts:Socket_opts.t
          * max_pending_requests:int
          * Sockaddr.t)
         @ local
      -> Listener.t @ p unique
    [@@mode p = (nonportable, portable)]

    val datagram_addrs
      :  #(Io.t * t * host:string * service:string) @ local
      -> Sockaddr.t list

    val%template bind_datagram_socket_or_cancel_exn
      :  #(Io.t * Cancellation.t * t * opts:Socket_opts.t * Sockaddr.t) @ local
      -> Datagram_socket.t @ p unique
    [@@mode p = (nonportable, portable)]
  end

  type 's t = (module S with type t = 's)
end

module type Net = sig @@ portable
  (** Abstract network interface for establising connected point-to-point streams and
      creating connectionless datagram sockets. *)

  (** {1 Interface} *)

  (** Represents a network interface for establising connected point-to-point streams and
      creating connectionless datagram sockets. *)
  type t : value mod contended many non_float unyielding

  (** [addr_info t sockaddr] determines the [host] name and [service] name corresponding
      to the [sockaddr]. *)
  val addr_info
    :  Io.t @ local
    -> t @ local
    -> Sockaddr.t @ local
    -> #(host:string * service:string)

  (** {2 Stream sockets} *)

  (** [stream_addrs io t ~host ~service] determines a list of addresses for the specified
      [host] and [service] that are suitable for connection oriented protocols. *)
  val stream_addrs
    :  Io.t @ local
    -> t @ local
    -> host:string @ local
    -> service:string @ local
    -> Sockaddr.t list

  (** {3 Connecting} *)

  (** [connect io t ~opts sockaddr ~f] is {{!connect_owned}
      [Duplex.use io (Duplex.Owned.unwrap (connect_owned io t ~opts sockaddr))]} and
      ensures that no resources are leaked. *)
  val connect
    :  Io.t @ local
    -> t @ local
    -> opts:Socket_opts.t @ local
    -> Sockaddr.t @ local
    -> f:(Duplex.t# @ local -> 'r @ once unique) @ local once
    -> 'r @ once unique

  include sig
    (** @closed *)

    (** [connect_owned io t ~opts sockaddr] connects to service at specified [sockaddr]. *)
    val connect_owned
      :  Io.t @ local
      -> t @ local
      -> opts:Socket_opts.t @ local
      -> Sockaddr.t @ local
      -> Duplex.Owned.t# @ unique

    (** [connect_owned_or_cancel io cancellation t ~opts addr] is {{!connect_owned}
        [Completed (connect_owned io t ~opts addr)]} unless the [cancellation] token was
        canceled before the server accepted in which case the result is [Canceled].

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before the server accepted. *)
    val connect_owned_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local
      -> opts:Socket_opts.t @ local
      -> Sockaddr.t
      -> (Duplex.Owned.t# Or_canceled.t[@kind value_or_null & value_or_null]) @ unique
  end

  (** {3 Listening} *)

  (** [listen io t ~opts ~max_pending_requests sockaddr ~f] is {{!listen_unique}
      [Listener.use io ~f @@ listen_unique io t ~opts ~max_pending_requests]} which
      ensures that no resources are leaked. *)
  val%template listen
    : ('r : value_or_null).
    Io.t @ local
    -> t @ local p
    -> opts:Socket_opts.t @ local
    -> max_pending_requests:int @ local
    -> Sockaddr.t @ local
    -> f:(Listener.t @ local p -> 'r @ once unique) @ local once
    -> 'r @ once unique
  [@@mode p = (nonportable, portable)]

  include sig
    (** @closed *)

    (** [listen_unique io t ~opts ~max_pending_requests sockaddr] creates a socket that is
        bound to the specified [sockaddr] and configured for listening to incoming
        connections. *)
    val%template listen_unique
      :  Io.t @ local
      -> t @ local p
      -> opts:Socket_opts.t @ local
      -> max_pending_requests:int @ local
      -> Sockaddr.t @ local
      -> Listener.t @ p unique
    [@@mode p = (nonportable, portable)]

    (** [listen_or_cancel_unique io cancellation t ~opts ~max_pending_requests addr] is
        {{!listen_unique}
        [Completed (listen_unique io t ~opts ~max_pending_requests addr)]} unless the
        [cancellation] token was canceled before the listener was bound in which case the
        result is [Canceled].

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before the listener was bound. *)
    val%template listen_unique_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local portable
      -> opts:Socket_opts.t @ local
      -> max_pending_requests:int @ local
      -> Sockaddr.t @ local
      -> Listener.t Or_canceled.t @ p unique
    [@@mode p = (nonportable, portable)]
  end

  (** {2 Datagram sockets} *)

  (** [datagram_addrs io t ~host ~service] determines a list of addresses for the
      specified [host] and [service] that are suitable for connectionless protocols. *)
  val datagram_addrs
    :  Io.t @ local
    -> t @ local
    -> host:string @ local
    -> service:string @ local
    -> Sockaddr.t list

  (** {3 Binding} *)

  (** [bind_datagram_socket io t ~opts sockaddr ~f] is {{!bind_datagram_socket_unique}
      [Datagram_socket.use io ~f @@ bind_datagram_socket_unique op t ~opts sockaddr]}
      which ensures that no resources are leaked. *)
  val%template bind_datagram_socket
    : ('r : value_or_null).
    Io.t @ local
    -> t @ local p
    -> opts:Socket_opts.t @ local
    -> Sockaddr.t @ local
    -> f:(Datagram_socket.t @ local p -> 'r @ once unique) @ local once
    -> 'r @ once unique
  [@@mode p = (nonportable, portable)]

  include sig
    (** @closed *)

    (** [bind_datagram_socket_unique io t ~opts sockaddr] creates a socket that is bound
        to the specified [sockaddr] and configured for connectionless communication. *)
    val%template bind_datagram_socket_unique
      :  Io.t @ local
      -> t @ local p
      -> opts:Socket_opts.t @ local
      -> Sockaddr.t @ local
      -> Datagram_socket.t @ p unique
    [@@mode p = (nonportable, portable)]

    (** [bind_datagram_socket_unique_or_cancel io cancellation t ~opts addr] is
        {{!bind_datagram_socket_unique}
        [Completed (bind_datagram_socket_unique io t ~opts addr)]} unless the
        [cancellation] token was canceled before the socket was bound in which case the
        result is [Canceled].

        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before the socket was bound. *)
    val%template bind_datagram_socket_unique_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> t @ local p
      -> opts:Socket_opts.t @ local
      -> Sockaddr.t @ local
      -> Datagram_socket.t Or_canceled.t @ p unique
    [@@mode p = (nonportable, portable)]
  end

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
