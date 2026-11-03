open Ox_kernel
open Await_kernel
open Filesystem_types
open Io_types

module Prim = struct
  module type S = sig
    type t : value mod contended portable

    val close : #(Await.t * t * Io_descr.t * Io_descr.Roles.t) @ local -> unit#

    val stream_addrs
      :  #(Await.t * t * host:string * service:string) @ local
      -> Sockaddr.t list

    val connect_or_cancel_exn
      :  #(Await.t * Cancellation.t * t * opts:Socket_opts.t * Sockaddr.t) @ local
      -> Io_descr.t @ unique

    val listen_or_cancel_exn
      :  #(Await.t
          * Cancellation.t
          * t
          * opts:Socket_opts.t
          * max_pending_requests:int
          * Sockaddr.t)
         @ local
      -> Io_descr.t @ unique

    val accept_or_cancel_exn
      :  #(Await.t * Cancellation.t * t * Io_descr.t * opts:Socket_opts.t) @ local
      -> #(fd:Io_descr.t * client_addr:Sockaddr.t) @ unique

    val datagram_addrs
      :  #(Await.t * t * host:string * service:string) @ local
      -> Sockaddr.t list

    val bind_datagram_socket_or_cancel_exn
      :  #(Await.t * Cancellation.t * t * opts:Socket_opts.t * bind_to:Sockaddr.t) @ local
      -> Io_descr.t @ unique

    (** Allows zero length operations. *)
    val recv_from_or_cancel_exn
      :  #(Await.t * Cancellation.t * t * Io_descr.t * Io_slice.t) @ local
      -> #(len:isize * sender_addr:Sockaddr.t)

    val is_nonblocking : #(Await.t * t * Io_descr.t) @ local -> bool

    (** Allows zero length operations. *)
    val recv_from_nonblocking
      :  #(Await.t * t * Io_descr.t * Io_slice.t) @ local
      -> #(len:Io_result.t * sender_addr:Sockaddr.t or_null)

    (** Allows zero length operations. *)
    val send_to_or_cancel_exn
      :  #(Await.t * Cancellation.t * t * Io_descr.t * Io_slice.read_only * Sockaddr.t)
         @ local
      -> isize

    (** Allows zero length operations. *)
    val send_to_nonblocking
      :  #(Await.t * t * Io_descr.t * Io_slice.read_only * Sockaddr.t) @ local
      -> Io_result.t

    val addr_info : #(Await.t * t * Sockaddr.t) @ local -> #(host:string * service:string)
    val addr : #(Await.t * t * Io_descr.t) @ local -> Sockaddr.t

    val read_stream_or_cancel_exn
      :  #(Await.t * Cancellation.t * t * Io_descr.t * Io_slice.t) @ local
      -> Io_result.t

    val read_stream_nonblocking
      :  #(Await.t * t * Io_descr.t * Io_slice.t) @ local
      -> Io_result.t

    val write_stream_or_cancel_exn
      :  #(Await.t * Cancellation.t * t * Io_descr.t * Io_slice.read_only) @ local
      -> Io_result.t

    val write_stream_nonblocking
      :  #(Await.t * t * Io_descr.t * Io_slice.read_only) @ local
      -> Io_result.t

    val wait_or_cancel_exn
      :  #(Await.t * Cancellation.t * t * Io_descr.t * Io_wait_flags.t) @ local
      -> Io_wait_flags.t

    (** Allows zero length operations. *)
    val read_seekable_or_cancel_exn
      :  #(Await.t * Cancellation.t * t * Io_descr.t * i64 * Io_slice.t) @ local
      -> isize

    (** Allows zero length operations. *)
    val write_seekable_or_cancel_exn
      :  #(Await.t * Cancellation.t * t * Io_descr.t * i64 * Io_slice.read_only) @ local
      -> isize

    val resize : #(Await.t * t * Io_descr.t * i64) @ local -> unit#
    val flush : #(Await.t * t * Io_descr.t) @ local -> unit#

    val%template stat : #(Await.t * t * Io_descr.t) @ local -> File_stats.t @ l
    [@@alloc a @ l = (heap_global, stack_local)]

    val current_dir : #(Await.t * t) @ local -> Io_descr.t @ aliased

    val open_dir_at
      :  #(Await.t * t * relative_to:Io_descr.t * follow_link:bool * File_path.t) @ local
      -> Io_descr.t @ unique

    val%template read_dir_at
      :  #(Await.t * t * relative_to:Io_descr.t * follow_link:bool * File_path.t) @ local
      -> File_path.Part.t iarray @ l
    [@@alloc a @ l = (heap_global, stack_local)]

    val%template stat_at
      :  #(Await.t * t * relative_to:Io_descr.t * follow_link:bool * File_path.t) @ local
      -> File_stats.t @ l
    [@@alloc a @ l = (heap_global, stack_local)]

    val create_dir_at
      :  #(Await.t
          * t
          * relative_to:Io_descr.t
          * File_path.t
          * parents:bool
          * File_permissions.t)
         @ local
      -> unit#

    val remove_dir_at
      :  #(Await.t * t * relative_to:Io_descr.t * File_path.t) @ local
      -> unit#

    val remove_file_at
      :  #(Await.t * t * relative_to:Io_descr.t * File_path.t) @ local
      -> unit#

    val rename_at
      :  #(Await.t
          * t
          * relative_to_before:Io_descr.t
          * before:File_path.t
          * relative_to_after:Io_descr.t
          * after:File_path.t)
         @ local
      -> unit#

    val create_link_at
      :  #(Await.t * t * into:string * relative_to:Io_descr.t * File_path.t) @ local
      -> unit#

    val%template read_link_at
      :  #(Await.t * t * relative_to:Io_descr.t * File_path.t) @ local
      -> string @ l
    [@@alloc a @ l = (heap_global, stack_local)]

    val open_file_to_read_at
      :  #(Await.t * t * relative_to:Io_descr.t * follow_link:bool * File_path.t) @ local
      -> Io_descr.t @ unique

    val open_or_create_file_at
      :  #(Await.t
          * t
          * create:File_create.t
          * relative_to:Io_descr.t
          * follow_link:bool
          * File_path.t)
         @ local
      -> Io_descr.t @ unique

    val open_or_create_file_to_append_at
      :  #(Await.t
          * t
          * create:File_create.t
          * relative_to:Io_descr.t
          * follow_link:bool
          * File_path.t)
         @ local
      -> Io_descr.t @ unique
  end

  type 'c t = (module S with type t = 'c)
end

module type Io = sig @@ portable
  (** Abstract system level IO implementation.

      The [Io] capability is an interface to system level IO implementations. It is
      intended to be implementable differently for different schedulers such as [Async]
      and other schedulers providing a [Concurrent.t] implementation.

      [Io] implementations share a concrete file descriptor type {{!Expert.Io_descr}
      [Io_descr]}. This makes it possible to have portable implementations of IO
      abstractions that can be moved to be used on a different thread of control
      potentially running under a different scheduler and using a different [Io]
      implementation. *)

  (** {1 Interface} *)

  type inner : (value & value) mod contended many non_float portable

  (** Represents the capability to perform IO.

      [t] is the type of implementations of system level direct-style IO operations. IO
      operations take a [t] that provides both the system level IO operations as well as
      an {!Await.t}. *)
  type t = (inner Capability.t[@kind value_or_null & value_or_null])

  (** [await io] is the {!Await.t} of the IO implementation. *)
  val await : t @ local -> Await.t @ local

  (** [with_terminator io terminator] is like [io] except with the terminator of the
      {!Await.t} within [io] replaced with the given [terminator]. *)
  val with_terminator : t @ local -> Terminator.t @ local -> t @ local

  (** {1 Expert interface for implementations} *)

  include sig
    (** @closed *)

    (** These interfaces are intended for implementations only. *)

    module Prim = Prim (** @inline *)

    module Expert : sig
      module Io_result = Io_result (** @inline *)

      val create
        : ('s : value mod contended portable).
        Await.t @ local
        -> 'c Prim.t @ local portable unyielding
        -> 'c @ local unyielding
        -> t @ local

      (** {1 Close} *)

      val close : #(t * Io_descr.t * Io_descr.Roles.t) @ local -> unit#
      val close_readable : #(t * Io_descr.t) @ local -> unit#
      val close_writable : #(t * Io_descr.t) @ local -> unit#
      val close_bidirect : #(t * Io_descr.t) @ local -> unit#

      (** {1 Network} *)

      val addr_info : #(t * Sockaddr.t) @ local -> #(host:string * service:string)
      val addr : #(t * Io_descr.t) @ local -> Sockaddr.t

      (** {2 Stream sockets} *)

      val stream_addrs : #(t * host:string * service:string) @ local -> Sockaddr.t list

      val connect_or_cancel_exn
        :  #(t * Cancellation.t * opts:Socket_opts.t * Sockaddr.t) @ local
        -> Io_descr.t

      val listen_or_cancel_exn
        :  #(t
            * Cancellation.t
            * opts:Socket_opts.t
            * max_pending_requests:int
            * Sockaddr.t)
           @ local
        -> Io_descr.t

      (* *)

      val accept_or_cancel_exn
        :  #(t * Cancellation.t * Io_descr.t * opts:Socket_opts.t) @ local
        -> #(fd:Io_descr.t * client_addr:Sockaddr.t) @ unique

      (** {2 Datagram sockets} *)

      val datagram_addrs : #(t * host:string * service:string) @ local -> Sockaddr.t list

      val bind_datagram_socket_or_cancel_exn
        :  #(t * Cancellation.t * opts:Socket_opts.t * bind_to:Sockaddr.t) @ local
        -> Io_descr.t

      val is_nonblocking : #(t * Io_descr.t) @ local -> bool

      (** Allows zero length operations. *)
      val send_to_or_cancel_exn
        :  #(t * Cancellation.t * Io_descr.t * Io_slice.read_only * Sockaddr.t) @ local
        -> isize

      (** Allows zero length operations. *)
      val send_to_nonblocking
        :  #(t * Io_descr.t * Io_slice.read_only * Sockaddr.t) @ local
        -> Io_result.t

      (** Allows zero length operations. *)
      val recv_from_or_cancel_exn
        :  #(t * Cancellation.t * Io_descr.t * Io_slice.t) @ local
        -> #(len:isize * sender_addr:Sockaddr.t)

      (** Allows zero length operations. *)
      val recv_from_nonblocking
        :  #(t * Io_descr.t * Io_slice.t) @ local
        -> #(len:Io_result.t * sender_addr:Sockaddr.t or_null)

      (** {1 Streams} *)

      val read_stream_or_cancel_exn
        :  #(t * Cancellation.t * Io_descr.t * Io_slice.t) @ local
        -> Io_result.t

      val read_stream_nonblocking : #(t * Io_descr.t * Io_slice.t) @ local -> Io_result.t

      val write_stream_or_cancel_exn
        :  #(t * Cancellation.t * Io_descr.t * Io_slice.read_only) @ local
        -> Io_result.t

      val write_stream_nonblocking
        :  #(t * Io_descr.t * Io_slice.read_only) @ local
        -> Io_result.t

      val wait_or_cancel_exn
        :  #(t * Cancellation.t * Io_descr.t * Io_wait_flags.t) @ local
        -> Io_wait_flags.t

      val wait_read_or_cancel_exn : #(t * Cancellation.t * Io_descr.t) @ local -> unit
      val wait_write_or_cancel_exn : #(t * Cancellation.t * Io_descr.t) @ local -> unit

      (** {1 Seekables} *)

      (** Allows zero length operations. *)
      val read_seekable_or_cancel_exn
        :  #(t * Cancellation.t * Io_descr.t * offset:i64 * Io_slice.t) @ local
        -> isize

      (** Allows zero length operations. *)
      val write_seekable_or_cancel_exn
        :  #(t * Cancellation.t * Io_descr.t * offset:i64 * Io_slice.read_only) @ local
        -> isize

      (* *)

      val resize : #(t * Io_descr.t * i64) @ local -> unit#
      val flush : #(t * Io_descr.t) @ local -> unit#

      val%template stat : #(t * Io_descr.t) @ local -> File_stats.t @ l
      [@@alloc a @ l = (heap_global, stack_local)]

      (** {1 File system} *)

      val current_dir : t @ local -> Io_descr.t

      val open_dir_at
        :  #(t * relative_to:Io_descr.t * follow_link:bool * File_path.t) @ local
        -> Io_descr.t

      val%template read_dir_at
        :  #(t * relative_to:Io_descr.t * follow_link:bool * File_path.t) @ local
        -> File_path.Part.t iarray @ l
      [@@alloc a @ l = (heap_global, stack_local)]

      val%template stat_at
        :  #(t * relative_to:Io_descr.t * follow_link:bool * File_path.t) @ local
        -> File_stats.t @ l
      [@@alloc a @ l = (heap_global, stack_local)]

      val create_dir_at
        :  #(t * relative_to:Io_descr.t * File_path.t * parents:bool * File_permissions.t)
           @ local
        -> unit#

      val remove_dir_at : #(t * relative_to:Io_descr.t * File_path.t) @ local -> unit#
      val remove_file_at : #(t * relative_to:Io_descr.t * File_path.t) @ local -> unit#

      val rename_at
        :  #(t
            * relative_to_before:Io_descr.t
            * before:File_path.t
            * relative_to_after:Io_descr.t
            * after:File_path.t)
           @ local
        -> unit#

      val create_link_at
        :  #(t * into:string * relative_to:Io_descr.t * File_path.t) @ local
        -> unit#

      val%template read_link_at
        :  #(t * relative_to:Io_descr.t * File_path.t) @ local
        -> string @ l
      [@@alloc a @ l = (heap_global, stack_local)]

      val open_file_to_read_at
        :  #(t * relative_to:Io_descr.t * follow_link:bool * File_path.t) @ local
        -> Io_descr.t

      val open_or_create_file_at
        :  #(t
            * create:File_create.t
            * relative_to:Io_descr.t
            * follow_link:bool
            * File_path.t)
           @ local
        -> Io_descr.t

      val open_or_create_file_to_append_at
        :  #(t
            * create:File_create.t
            * relative_to:Io_descr.t
            * follow_link:bool
            * File_path.t)
           @ local
        -> Io_descr.t
    end
  end
end
