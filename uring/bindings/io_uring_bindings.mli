@@ portable

open Base
open Filesystem_types
open Ox
open Linux_ext

(** Low level bindings to [io_uring]. *)

module Result : sig
  (** Return value of an [io_uring] operation. *)

  include module type of struct
    include Core_unix.Syscall_result.Int
  end

  (** [is_more_specific x ~than:y] determines whether [x] is considered a more specific
      result than [y]. [Ok 0] is considered the least specific result, i.e. less specific
      than any other [Ok _] or [Error _], any [Error _] is more specific than any [Ok _],
      and [Error ECANCELED] is considered the least specific error. Otherwise results are
      unordered. *)
  val is_more_specific : t -> than:t -> bool

  (** [ecanceled] is [Error ECANCELED] except that [ECANCELED] is not currently a part of
      the [Core_unix.Error.t] variant type. *)
  val ecanceled : t

  (** [is_already result] determines whether the [result] indicates that the operation
      failed with [EALREADY]. *)
  val is_already : t -> bool

  (** [is_canceled result] determines whether the [result] indicates that the operation
      failed with [ECANCELED]. *)
  val is_canceled : t -> bool

  (** [is_exist result] determines whether the [result] indicates that the operation
      failed with [EEXIST]. *)
  val is_exist : t -> bool

  (** [is_isdir result] determines whether the [result] indicates that the operation
      failed with [EISDIR]. *)
  val is_isdir : t -> bool

  (** [is_noent result] determines whether the [result] indicates that the operation
      failed with [ENOENT]. *)
  val is_noent : t -> bool
end

module Tag : sig
  (** User defined tags for [io_uring] requests. *)

  (** User defined tags for [io_uring] requests. *)
  type t = nativeint_u
end

module Socklen_ref : sig
  (** Space to hold a [socklen_t]. *)

  (** Space to hold a [socklen_t]. *)
  type t : bits64 mod external_ many stateless unyielding

  (** [empty #()] returns a fresh socket length ref allocated from the stack. *)
  val empty : unit# -> t @ local

  (** [unsafe_globalize t] is [t] as global, which must not outlive the local [t]. *)
  val unsafe_globalize : t @ local -> t
end

module Sockaddr_ref : sig
  (** Space to hold a socket address, e.g. [sockaddr_in]. *)

  (** Space to hold a socket address, e.g. [sockaddr_in]. *)
  type t : (bits64 & word) mod external_ many stateless unyielding

  (** [for_sockaddr #()] return a fresh a socket address ref allocated from the stack with
      enough space to hold a [Sockaddr.t], i.e. an IPv4 INET address. *)
  val for_sockaddr : unit# -> t @ local

  (** [set_socklen t socklen_ref] updates [socklen_ref] to have the length of the socket
      address [t]. *)
  val set_socklen : t @ local read -> Socklen_ref.t @ local -> unit#

  (** [set_to_sockaddr t sockaddr] stores the [sockaddr] in the socket address ref.

      @raise Invalid_argument
        if [t] does not have space for a [Sockaddr.t], i.e. an IPv4 INET address. *)
  val set_to_sockaddr : t @ local -> Sockaddr.t -> unit#

  (** [get_as_sockaddr t socklen_ref]

      @raise Invalid_argument
        if the [t] doesn't have enough space for a [Sockaddr.t] or its length disagrees
        with the [socklen_ref] or if socket address ref doesn't have the correct address
        family. *)
  val get_as_sockaddr
    :  t @ local read
    -> Socklen_ref.t @ local read
    -> Sockaddr.t @ unique

  (** [unsafe_globalize t] is [t] as global, which must not outlive the local [t]. *)
  val unsafe_globalize : t @ local -> t
end

module Iovec : sig
  (** Space to hold an array of [iovec]s, i.e. references to memory buffers for IO. *)

  (** Space to hold an array of [iovec]s, i.e. references to memory buffers for IO. *)
  type t : (bits64 & word) mod external_ many stateless unyielding

  (** [make bufs] allocates an array of [iovec]s from the stack to reference the specified
      buffers. *)
  val%template make
    :  (Buf.t Modes.t[@modality global] [@kind bits64 & word]) Iarray.t @ local v
    -> t @ local v
  [@@mode v = (read_write, read)]

  (** [unsafe_globalize t] is [t] as global, which must not outlive the local [t]. *)
  val%template unsafe_globalize : t @ local v -> t @ v
  [@@mode v = (read_write, read)]
end

module Msghdr : sig
  (** Space to hold a message header, i.e. [msghdr] for [recvmsg] or [sendmsg]. *)

  (** Space to hold a message header, i.e. [msghdr] for [recvmsg] or [sendmsg]. *)
  type t : bits64 mod external_ many stateless unyielding

  (** [make sockaddr_ref iovec] is a message header referring to the given socket address
      ref [sockadd_ref] and array of buffers [iovec]. *)
  val%template make : Sockaddr_ref.t @ v -> Iovec.t @ v -> t @ local v
  [@@mode v = (read_write, read)]

  (** [socklen t] is the socket length field of the message header [t]. *)
  val socklen : t @ local -> Socklen_ref.t @ local

  (** [unsafe_globalize t] is [t] as global, which must not outlive the local [t]. *)
  val%template unsafe_globalize : t @ local v -> t @ v
  [@@mode v = (read_write, read)]
end

module Statx_ref : sig
  (** Space to hold file stats, i.e. [statx]. *)

  (** Space to hold file stats, i.e. [statx]. *)
  type t : bits64 mod external_ many stateless unyielding

  (** [empty #()] returns a fresh file stats ref allocated from the stack. *)
  val empty : unit# -> t @ local

  (** [with_malloc ~f] calls [f] with a fresh file stats ref allocated from the C heap. *)
  val%template with_malloc : f:(t @ local -> 'a @ l) @ local once -> 'a @ l
  [@@alloc a @ l = (heap_global, stack_local)]

  (** [get t] reads the file stats ref and converts the values to a [File_stats.t]. *)
  val%template get : t @ local -> File_stats.t @ l
  [@@alloc a @ l = (heap_global, stack_local)]

  val get_kind : t @ local -> File_kind.t

  (** [unsafe_globalize t] is [t] as global, which must not outlive the local [t]. *)
  val unsafe_globalize : t @ local -> t
end

module String_ref : sig
  (** Space to hold a string. *)

  (** Space to hold a string. *)
  type t : (bits64 & word) mod external_ many stateless unyielding

  (** [of_string string] allocates a fresh string ref large enough for the given string
      from the stack and copies the string to it. *)
  val of_string : string @ local -> t @ local

  (** [of_file_path path] allocates a fresh string ref large enough for the given path
      from the stack and copies the path to it. *)
  val of_file_path : File_path.t @ local -> t @ local

  (** [unsafe_globalize t] is [t] as global, which must not outlive the local [t]. *)
  val unsafe_globalize : t @ local -> t
end

module Open_mode : sig
  (** Modes for [openat] operations.

      Please refer to the documentation of [openat(2)]. *)

  type t : immediate64

  val rdonly : t
  val wronly : t
  val rdwr : t
end

module Open_flags : sig
  (** Modifier flags for [openat] operations.

      Please refer to the documentation of [openat(2)]. *)

  type t : immediate64 [@@deriving sexp_of]

  include Flags.S with type t := t

  (** {1 Creation flags} *)

  val cloexec : t
  val creat : t
  val directory : t
  val excl : t
  val noctty : t
  val nofollow : t
  val tmpfile : t
  val trunc : t

  (** {1 Status flags} *)

  val append : t
  val async : t
  val direct : t
  val dsync : t
  val largefile : t
  val noatime : t
  val nonblock : t
  val path : t
  val sync : t
end

module Socket_domain : sig
  (** Socket domains for [socket] operations.

      Please refer to the documentation of [socket(2)]. *)

  type t : immediate64

  val unix : t
  val inet : t
  val inet6 : t
end

module Socket_type : sig
  (** Socket types for [socket] operations.

      Please refer to the documentation of [socket(2)]. *)

  type t : immediate64

  val stream : t
  val dgram : t
  val seqpacket : t
  val raw : t
end

module Socket_flags : sig
  (** Socket flags for operations creating sockets, e.g. [socket] and [accept].

      Please refer to the documentation of [socket(2)] and [accept4(2)]. *)

  type t : immediate64 [@@deriving sexp_of]

  include Flags.S with type t := t

  val nonblock : t
  val cloexec : t
end

module Recv_flags : sig
  (** Flags for receive operations.

      Please refer to the documentation of [recv(2)] or [recvmsg(2)]. *)

  type t : immediate64 [@@deriving sexp_of]

  include Flags.S with type t := t

  val dontwait : t
  val oob : t
  val peek : t
  val trunc : t
  val waitall : t
end

module Send_flags : sig
  (** Flags for send operations.

      Please refer to the documentation of [send(2)] or [sendmsg(2)]. *)

  type t : immediate64 [@@deriving sexp_of]

  include Flags.S with type t := t

  val dontwait : t
  val oob : t
  val confirm : t
  val dontroute : t
  val eor : t
  val more : t
  val nosignal : t
  val fastopen : t
end

module Statx_flags : sig
  (** Flags for [statx] operations.

      Please refer to the documentation of [statx(2)]. *)

  type t : immediate64 [@@deriving sexp_of]

  include Flags.S with type t := t

  val basic_stats : t
  val type_ : t
end

module At_flags : sig
  (** Flags for at operations, e.g. [statx] and [unlinkat].

      Please refer to the documentation of [statx(2)] and [unlinkat(2)]. *)

  type t : immediate64 [@@deriving sexp_of]

  include Flags.S with type t := t

  val symlink_nofollow : t
  val removedir : t
end

module Sqe_flags : sig
  (** Flags for {!Sq} entries.

      Please refer to the documentation of [io_uring_sqe_set_flags(3)]. *)

  type t : immediate64 [@@deriving sexp_of]

  include Flags.S with type t := t

  val async : t
  val cqe_skip_success : t
  val io_drain : t
  val io_hardlink : t
  val io_link : t
end

module Cancel_flags : sig
  (** Flags for cancellation requests.

      Please refer to the documentation of [io_uring_prep_cancel(3)]. *)

  type t : immediate64 [@@deriving sexp_of]

  include Flags.S with type t := t

  val all : t
  val any : t
  val userdata : t
end

module Sq : sig
  (** Submission queue of an [io_uring].

      Intended usage pattern:

      {[
        ensure_can_push sq ~n;
        push sq _1;
        (* ... *)
        push sq _n;
        submit sq
      ]}

      First ensure there will be enough space in the queue to push all the requests that
      you want to submit at a time. The {!submit} at the end ensures that the requests are
      visible to and will be consumed by the [io_uring] backend. It is safe to omit the
      {!submit} (and perform it at some later point) in case you do not want to be able to
      wait for the completions. A {!submit} is strictly necessary at the point after which
      you want to be able to wait for the completions. *)

  (** Submission queue of an [io_uring]. *)
  type t : value mod aliased portable unyielding

  (** [entries sq] is the maximum number of requests that the submission queue can hold. *)
  val entries : (t, 'k) Capsule.Data.t @ local -> int

  (** {1 Queue space management} *)

  (** [submit sq] ensures that any previously pushed requests are visible to and will be
      consumed by the [io_uring] backend. *)
  val submit : t Capsule.Scoped.t @ local -> unit

  (** [ensure_can_push sq ~n] ensures that [n] requests can be pushed to the submission
      queue without having to {!submit}. If necessary, this will call {!submit}. *)
  val ensure_can_push : t Capsule.Scoped.t @ local -> n:int -> unit

  (** {1 Pushing requests}

      Push operations terminate the program or block indefinitely in case the queue cannot
      have space for the request.

      Each [push_xxx t ~tag sqe_flags ...] operation corresponds to a [io_uring_prep_xxx]
      call preceded by logic to obtain a SQE and followed by setting the data to [tag] and
      the flags to [sqe_flags]. Please refer to the documentation of the
      [io_uring_prep_xxx(3)] in question for details on the parameters. *)

  val push_cancel
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> tag_to_cancel:Tag.t
    -> Cancel_flags.t
    -> unit

  val push_nop : t Capsule.Scoped.t @ local -> tag:Tag.t -> Sqe_flags.t -> unit

  val push_poll_add
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> Epoll.Flags.t
    -> unit

  val push_close
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> unit

  val push_shutdown
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> Core_unix.shutdown_command
    -> unit

  val push_socket
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Socket_domain.t
    -> Socket_type.t
    -> Socket_flags.t
    -> protocol:int
    -> unit

  val push_connect
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> Sockaddr_ref.t @ read
    -> Socklen_ref.t @ read
    -> unit

  (** Requires [6.11] *)
  val push_bind
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> Sockaddr_ref.t @ read
    -> Socklen_ref.t @ read
    -> unit

  (** Requires [6.11] *)
  val push_listen
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> max_pending_requests:int
    -> unit

  val push_accept
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> Sockaddr_ref.t
    -> Socklen_ref.t
    -> Socket_flags.t
    -> unit

  val push_recvmsg
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> Msghdr.t
    -> Recv_flags.t
    -> unit

  val push_sendmsg
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> Msghdr.t @ read
    -> Send_flags.t
    -> unit

  val push_read
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> offset:I64.t
    -> Buf.t
    -> unit

  val push_write
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> offset:I64.t
    -> Buf.t @ read
    -> unit

  val push_fsync
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> unit

  (** Requires [6.9]. *)
  val push_ftruncate
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> Core_unix.File_descr.t
    -> I64.t
    -> unit

  val push_openat
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> relative_to:Core_unix.File_descr.t
    -> path:String_ref.t @ read
    -> Open_mode.t
    -> Open_flags.t
    -> File_permissions.t
    -> unit

  val push_unlinkat
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> relative_to:Core_unix.File_descr.t
    -> path:String_ref.t @ read
    -> At_flags.t
    -> unit

  val push_renameat
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> relative_to_before:Core_unix.File_descr.t
    -> before:String_ref.t @ read
    -> relative_to_after:Core_unix.File_descr.t
    -> after:String_ref.t @ read
    -> unit

  val push_symlinkat
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> into:String_ref.t @ read
    -> relative_to:Core_unix.File_descr.t
    -> path:String_ref.t @ read
    -> unit

  val push_mkdirat
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> relative_to:Core_unix.File_descr.t
    -> path:String_ref.t @ read
    -> File_permissions.t
    -> unit

  val push_statx
    :  t Capsule.Scoped.t @ local
    -> tag:Tag.t
    -> Sqe_flags.t
    -> relative_to:Core_unix.File_descr.t
    -> path:String_ref.t @ read
    -> At_flags.t
    -> Statx_flags.t
    -> Statx_ref.t
    -> unit
end

module Cq : sig
  (** Completion queue of an [io_uring]. *)

  (** Completion queue of an [io_uring]. *)
  type t : value mod aliased portable unyielding

  (** [register_eventfd cq eventfd] registers the [eventfd] to be incremented when new
      entries are added to the completion queue. *)
  val register_eventfd : t Capsule.Scoped.t @ local -> Eventfd.t -> Result.t

  type ('a : word & bits64) list =
    | []
    | ( :: ) of 'a * 'a list

  module Entry : sig
    (** Completion queue entry.

        Please refer to the documentation of [io_uring]. *)

    type t : (word & bits64) mod global immutable many stateless

    val tag : t -> Tag.t
    val result : t -> Result.t

    module Flags : sig
      (** Completion queue entry flags (excluding buffer index bits).

          Please refer to the documentation of [io_uring]. *)

      type t : immediate64 [@@deriving sexp_of]

      include Flags.S with type t := t

      val buffer : t
      val more : t
      val sock_nonempty : t
      val notif : t
      val buf_more : t
    end

    val flags : t -> Flags.t

    (** [buffer t] is the buffer index or [Null] if {!Flags.buffer} is not set. *)
    val buffer : t -> int or_null
  end

  (** [drain cq] removes all available entries from the completion queue and returns a
      list of them. *)
  val drain : t @ local -> Entry.t list @ local unique
end

(** [create ~attach ~sq_entries ~cq_entries ~sqpoll ()] allocates memory for and setups a
    ring. The returned ring should be destroyed by calling {!exit}. *)
val create
  :  attach:(Sq.t Capsule.Scoped.t Option.t[@kind value_or_null & void]) @ local
  -> sq_entries:int or_null
  -> cq_entries:int or_null
  -> sqpoll:(idle_ms:int * cpu_aff:int or_null) option @ local
  -> unit
  -> #(sq:Sq.t Capsule.Owned.t * cq:Cq.t Capsule.Owned.t) @ unique
[@@alert
  experimental
    "Io_uring_bindings should currently only be used internally by the Io_in_uring \
     library.  The interface of Io_uring_bindings is likely to go through some \
     substantial changes soon to support additional ways to use io_uring."]

(** [exit #(~sq, ~cq)] destroys the ring and frees the memory allocated for it. This
    should only be called when the ring is idle. *)
val exit : #(sq:Sq.t Capsule.Owned.t * cq:Cq.t Capsule.Owned.t) @ unique -> unit
