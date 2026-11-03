@@ portable

open Base

(** Flags for waiting on an IO abstraction. *)

(** Immutable set of flags for blocking on an IO abstraction. *)
type t = private Int63.t [@@deriving sexp_of]

(** {1 Flag constants} *)

(** Associated abstraction should be readable without blocking. *)
val in_ : t

(** Associated abstraction should be writable without blocking. *)
val out : t

(** {1 Flags interface} *)

include Flags.S with type t := t (** @closed *)
