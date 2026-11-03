@@ portable

open Ox_kernel

(** Buffer type for IO. *)

(** Represents a sequence of bytes in either internal or external memory for IO
    operations. *)
type t = U8.t Slice.t

(** [read_only] is essentially a workaround for lack of first-class modalities, i.e.
    [t @@ read]. *)
type read_only = (t Base.Modes.t[@modality read] [@kind (value_or_null & bits64) & word])

(** [assert_non_empty t] raises [Invalid_argument] iff the buffer [t] is empty. *)
val assert_non_empty : t @ immutable local -> unit

module Ext : sig
  (** Represents a sequence of bytes in external memory for IO operations. *)
  type t = U8.t Slice.Ext.t

  (** [read_only] is essentially a workaround for lack of first-class modalities, i.e.
      [t @@ read]. *)
  type read_only = (t Base.Modes.t[@modality read] [@kind bits64 & word])
end
