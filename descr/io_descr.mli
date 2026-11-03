@@ portable

(** Reference counted file descriptor. Separate reference counts are maintained for
    {{!Roles.readable} [readable]} and {{!Roles.writable} [writable]} roles.

    Reference counting is used as a single file descriptor may be referred to via multiple
    objects. In particular, a single socket file descriptor will be exposed as a pair of a
    readable and a writable. Also, any number of readables and writables may be obtained
    from a single file descriptor corresponding to a seekable file. *)

(** Represents a reference counted file descriptor. *)
type t : sync_data

(** [raw_fd] is [Core_unix.File_descr.t] on native.

    On [js_of_ocaml] and [wasm_of_ocaml], values of type [raw_fd] cannot be created. *)
type raw_fd : immediate

module Roles : sig
  (** Specifies roles for a reference counted file descriptor or an operation on file
      descriptor. *)

  type t : immediate64 [@@deriving sexp_of]

  include Flags.S with type t := t

  (** Refers to the readable side of a descriptor. *)
  val readable : t

  (** Refers to the writable side of a descriptor. *)
  val writable : t

  val empty : [ `Should_not_be_used ]
end

(** [create ~shutdown roles raw_fd] creates a new reference counted file descriptor with
    the reference count for [roles] of {{!Roles.readable} [readable]} or
    {{!Roles.writable} [writable]} or both.

    The [shutdown] flag is stored with the descriptor and considered at {!decr} to
    determine whether the read and/or the write direction of a full-duplex connection
    should be shut down as the reference count for the direction became zero. *)
val create : Roles.t -> shutdown:bool -> raw_fd -> t @ unique

(** [globalize t] is [t @ global]. It is implemented as the identity function; however, we
    don't expose that [t] crosses locality because most of the time it can be used
    [@ local] and this may help to avoid mistakes. *)
val globalize : t @ local -> t @@ stateless

(** [unsafe_get t] is the raw file descriptor value that the reference counted file
    descriptor was created with. The reference count is not checked. *)
val unsafe_get : t @ immutable local -> raw_fd @@ stateless

(** [incr t roles] increments the reference count for the specified [roles].

    @raise Failure
      in debug mode in case the reference count would overflow or was already zero. *)
val incr : Roles.t -> t @ local -> unit

module Actions : sig
  (** Specifies actions for the caller of {!decr} to take. *)

  type t : immediate64 [@@deriving sexp_of]

  include Flags.S with type t := t

  (** The readable side of the descriptor should be shutdown. *)
  val shutdown_rd : t

  (** The writable side of the descriptor should be shutdown. *)
  val shutdown_wr : t

  (** The descriptor should be closed after potentially shutting down sides. *)
  val and_close : t

  val empty : [ `Should_not_be_used ]
end

(** [decr t role] checks and potentially decrements the reference count for the specified
    [role] (or both) and determines whether the read and/or write side of a full-duplex
    connection should be first shut down and/or the descriptor closed.

    If the returned actions includes {{!Actions.shutdown_rd} [shutdown_rd]} or
    {{!Actions.shutdown_wr} [shutdown_wr]} and no {{!Actions.and_close} [and_close]}, then
    the caller should call {!should_close_after} after shutting down the roles to update
    the reference count and determine whether the descriptor should be closed.

    If {{!Actions.and_close} [and_close]} is included in the return actions, then
    {!should_close_after} should not be called.

    More concretely, the logic looks roughly like this:
    {[
      let actions = Io_descr.decr role fd in
      if Io_descr.Actions.(do_intersect shutdown_rd) actions
      then implementation_specific_shutdown_rd fd;
      if Io_descr.Actions.(do_intersect shutdown_wr) actions
      then implementation_specific_shutdown_wr fd;
      if Io_descr.Actions.(do_intersect and_close) actions
         || descr.should_close_after fd actions
      then implementation_specific_close fd
    ]}

    @raise Failure in debug mode in case a reference count was already zero. *)
val decr : Roles.t -> t @ local -> Actions.t

(** [should_close_after t actions] potentially decrements the reference counts for the
    roles that were shutdown and determines whether the descriptor should now be closed.

    @raise Failure
      in debug mode in case the [actions] included {!Actions.and_close} or in case a
      reference count was already zero. *)
val should_close_after : Actions.t -> t @ local -> bool
