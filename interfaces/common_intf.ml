open Io_kernel

module type Resource_with_perms = sig @@ portable
  (** Signature for a potentially parameterized IO abstraction that needs to be closed. *)

  type -'perms t : any

  include sig
    (** @closed *)

    (** Please prefer simpler leak-proof APIs whenever possible. *)

    (** [close io t] closes the resource [t]. *)
    val close : Io.t @ local -> 'perms t @ local unique -> unit#

    (** [use io t f] allows the resource [t] to be used locally in [f t] and then
        {{!close} closes} the resource after [f t] returns normally or raises an
        exception. *)
    val%template use
      : 'perms ('r : value_or_null).
      Io.t @ local
      -> 'perms t @ local p unique
      -> f:('perms t @ local p -> 'r @ once unique) @ local once
      -> 'r @ once unique
    [@@mode p = (nonportable, portable)]
  end
end

module type Resource = sig
  type t : any

  include Resource_with_perms with type 'perms t := t (** @inline *)
end
