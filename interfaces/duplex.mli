@@ portable

open Io_kernel
open Common_intf

(** Full-duplex {!Readable} and {!Writable} stream pairs. *)

(** {1 Interface} *)

(** [t] is a record of a [readable] and a [writable]. *)
type t =
  { readable : Readable.t
  ; writable : Writable.t
  }

module Owned : sig
  (** Pair of {!Readable} and {!Writable} each in their own separate {{!Owned.t} owned}
      capsule, allowing moving each independently. *)

  type unowned := t

  (** [t] is a record of an owned [readable] and an owned [writable]. *)
  type t =
    { readable : Readable.t Capsule.Owned.t
    ; writable : Writable.t Capsule.Owned.t
    }

  (** [unwrap owned] unwraps both the [readable] and the [writable], destroying each
      capsule and merging them into the current capsule. *)
  val unwrap : t# @ unique -> unowned# @ unique

  (** [close io t] unwraps the owned duplex [t] and closes both the readable and writable. *)
  val close : Io.t @ local -> t# @ unique -> unit#
end

(** {1 Resource interface} *)

include Resource with type t := t# (** @inline *)
