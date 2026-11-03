open Core
open Ox_kernel
open Await_kernel
open Io_types
open Io_kernel
open Common_intf

module Prim = struct
  module type S = sig
    type t

    val close : #(Io.t * t) @ local -> unit#
    val size : #(Io.t * t) @ local -> i64
    val resize : #(Io.t * t * i64) @ local -> unit#

    val%template readable : #(Io.t * t * offset:i64) @ local -> Readable.t @ p unique
    [@@mode p = (nonportable, portable)]

    val%template writable : #(Io.t * t * offset:i64) @ local -> Writable.t @ p unique
    [@@mode p = (nonportable, portable)]

    (** Allows empty results. *)
    val read_or_cancel_exn
      :  #(Io.t * Cancellation.t * t * offset:i64 * Io_slice.t) @ local
      -> isize

    (** Allows empty results. *)
    val write_or_cancel_exn
      :  #(Io.t * Cancellation.t * t * offset:i64 * Io_slice.read_only) @ local
      -> isize

    val flush : #(Io.t * t) @ local -> unit#
  end

  type 's t = (module S with type t = 's)
end

module type File = sig @@ portable
  (** Abstract readable and/or writable file of bytes. *)

  (** {1 Interface} *)

  (** Represents a file. *)
  type -'perms t : value mod contended many non_float unyielding

  (** {2 File size} *)

  (** [size io t] gets the size of the file [t]. *)
  val size : Io.t @ local -> [> read ] t @ local -> i64

  (** [resize io t new_size] sets the size of file [t]. If the new size is greater than
      the current size, then the contents of the extended area are not defined, i.e. the
      extended area is not necessarily zero filled. *)
  val resize : Io.t @ local -> [> write ] t @ local -> i64 @ local -> unit

  (** {2 Obtaining streams} *)

  (** {3 Readables} *)

  (** [readable io t ~offset ~f] is {{!readable_unique}
      [Readable.use io (readable_unique io t ~offset) ~f]} which ensures that no resources
      are leaked. *)
  val%template readable
    : 'perms ('r : value_or_null).
    Io.t @ local
    -> ([> read ] as 'perms) t @ local p
    -> offset:i64 @ local
    -> f:(Readable.t @ local p -> 'r @ once unique) @ local once
    -> 'r @ once unique
  [@@mode p = (nonportable, portable)]

  include sig
    (** @closed *)

    (** [readable_unique io t ~offset] is a new readable stream that will start reading
        onwards from the specified [offset].

        A readable obtained from a file likely does not support nonblocking operations. *)
    val%template readable_unique
      :  Io.t @ local
      -> [> read ] t @ local p
      -> offset:i64 @ local
      -> Readable.t @ p unique
    [@@mode p = (nonportable, portable)]
  end

  (** {3 Writables} *)

  (** [writable io t ~offset f] is {{!writable_unique}
      [Writable.use io (writable_unique io t ~offset) ~f]} which ensures that no resources
      are leaked. *)
  val%template writable
    : 'perms ('r : value_or_null).
    Io.t @ local
    -> ([> write ] as 'perms) t @ local p
    -> offset:i64 @ local
    -> f:(Writable.t @ local p -> 'r @ once unique) @ local once
    -> 'r @ once unique
  [@@mode p = (nonportable, portable)]

  include sig
    (** @closed *)

    (** [writable_unique io t ~offset] is a new writable stream that will start writing
        onwards from the specified [offset].

        A writable obtained from a file likely does not support nonblocking operations and
        is likely to allow extending the file beyond (current) end-of-file. *)
    val%template writable_unique
      :  Io.t @ local
      -> [> write ] t @ local p
      -> offset:i64 @ local
      -> Writable.t @ p unique
    [@@mode p = (nonportable, portable)]
  end

  (** {2 Blocking operations} *)

  (** {3 Reading} *)

  (** [read io t ~offset buffer] reads up to the whole [buffer] from the file at specified
      [offset], and returns the number of bytes read, which may be less than the length of
      the buffer.

      Empty operations are allowed. In particular, an attempt to read at or beyond the
      current end of the file returns zero. Note that you can call {!readable} to get a
      readable stream that does not allow empty operations.

      @raise Invalid_argument in case the buffer is empty.
      @raise Await_kernel.Await.Terminated
        in case [io] was terminated before any data transfer took place. *)
  val read
    :  Io.t @ local
    -> [> read ] t @ local
    -> offset:i64 @ local
    -> Io_slice.t @ local
    -> isize

  include sig
    (** @closed *)

    (** [read_or_cancel io cancellation t ~offset buffer] is {{!read}
        [Completed (read io t ~offset buffer)]} unless the [cancellation] token was
        canceled before any data was read in which case the result is [Canceled].

        @raise Invalid_argument in case the buffer is empty.
        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before any data was read. *)
    val%template read_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> [> read ] t @ local
      -> offset:i64 @ local
      -> Io_slice.t @ local
      -> (isize Or_canceled.t[@kind word]) @ l
    [@@mode l = (global, local)]
  end

  (** {3 Writing} *)

  (** [write io t ~offset buffer] writes up to the whole [buffer] to the file at specified
      [offset], and returns the number of bytes written, which can be less than the length
      of the buffer in case a limit for the file size is reached.

      Empty operations are allowed. For example, writing zero bytes at an offset beyond
      the current end-of-file will resize the file.

      @raise Invalid_argument in case the buffer is empty.
      @raise Await_kernel.Await.Terminated
        in case [io] was terminated before any data transfer took place. *)
  val write
    :  Io.t @ local
    -> [> write ] t @ local
    -> offset:i64 @ local
    -> Io_slice.t @ local read
    -> isize

  include sig
    (** @closed *)

    (** [write_or_cancel io cancellation t ~offset buffer] is {{!write}
        [Completed (write io t ~offset buffer)]} unless the [cancellation] token was
        canceled before any data was written in which case the result is [Canceled].

        @raise Invalid_argument in case the buffer is empty.
        @raise Await_kernel.Await.Terminated
          in case [io] was terminated before any data was written. *)
    val%template write_or_cancel
      :  Io.t @ local
      -> Cancellation.t @ local
      -> [> write ] t @ local
      -> offset:i64 @ local
      -> Io_slice.t @ local read
      -> (isize Or_canceled.t[@kind word]) @ l
    [@@mode l = (global, local)]
  end

  (** {2 Flushing} *)

  (** [flush io t] ensures that any writes to the file are transferred to the storage
      device (in an implementation defined manner) before returning. *)
  val flush : Io.t @ local -> [> write ] t @ local -> unit

  (** {1 Resource interface} *)

  include Resource_with_perms with type 'perms t := 'perms t (** @inline *)

  (** {1 Expert interface for implementations} *)

  include sig
    (** @closed *)

    (** These interfaces are intended for implementations only. *)

    module Prim = Prim (** @inline *)

    module Expert : sig
      val%template create
        : ('s : value mod contended).
        's @ p -> 's Prim.t @ p -> 'perms t @ p unique
      [@@mode p = (nonportable, portable)]
    end
  end
end
