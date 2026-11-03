open Await_kernel
open Io_kernel

module type Io_impl = sig @@ portable
  (** Signature for obtaining an [Io.t] implementation. *)

  module Config : sig
    type t : immutable_data

    val default : t
  end

  type t : value mod contended portable unyielding

  val create : Config.t -> t
  val pinned : Await.t @ local -> t @ local -> f:(unit# -> unit) @ local once -> unit

  val with_io
    :  Await.t @ local
    -> t @ local
    -> f:(Io.t @ local -> unit) @ local once
    -> unit

  module For_testing : sig
    val appears_idle : t @ local -> bool
  end
end

module Io_not_implemented = Io_not_implemented
