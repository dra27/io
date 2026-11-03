@@ portable

include module type of struct
  include Io_descr
end

module Actions : sig
  include module type of Actions

  (** [to_shutdown_command t] is [This shutdown_command] corresponding to the shutdown
      command to be taken or [Null] if [t] does not have shutdown flags set. *)
  val to_shutdown_command : t -> Core_unix.shutdown_command or_null
end

(** [create roles ~shutdown raw_fd] creates a new reference counted file descriptor with
    the reference count for [roles] of {{!Roles.readable} [readable]} or
    {{!Roles.writable} [writable]} or both initialized to [1].

    The [shutdown] flag is stored with the descriptor and considered at {!decr} to
    determine whether the read and/or the write direction of a full-duplex connection
    should be shut down as the reference count for the direction became zero. *)
val create : Roles.t -> shutdown:bool -> Core_unix.File_descr.t -> t @ unique

(** [unsafe_get t] is the raw file descriptor value that the reference counted file
    descriptor was created with. The reference count is not checked. *)
val unsafe_get : t @ local -> Core_unix.File_descr.t
