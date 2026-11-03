@@ portable

open Await
open Io_kernel

(** An [io_uring] based implementation of {!Io.t}. *)

(** [with_io await t ~f] calls [f] with an [Io.t].

    Note that this does not pin or create rings. *)
val with_io
  : ('r : value_or_null).
  Await.t @ local
  -> Io_uring_shards.t @ local
  -> f:(Io.t @ local -> 'r @ once unique) @ local once
  -> 'r @ once unique
