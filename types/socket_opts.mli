@@ portable

open Base

(** Concrete type for specifying options when creating new sockets.

    [Null] for any option means to keep the default specified by the implementation. *)
type t =
  { keep_alive : bool or_null (** Keep connection alive. *)
  ; permit_broadcast : bool or_null (** Permit sending of broadcast messages. *)
  ; linger : int option or_null (** Linger on closed connections that have data. *)
  ; recv_buffer : int or_null (** Size of input buffer. *)
  ; recv_timeout : float or_null (** Timeout for input operations. *)
  ; reuse_addr : bool or_null (** Allow reuse of local addresses. *)
  ; reuse_port : bool or_null (** Allow reuse of local ports. *)
  ; send_buffer : int or_null (** Size of output buffer. *)
  ; send_timeout : float or_null (** Timeout for output operations. *)
  ; nodelay : bool or_null
  (** Disable Nagle's algorithm for TCP sockets.

      This option is silently ignored on sockets that do not support Nagle's algorithm. *)
  }

(** {1 Constants for common use cases} *)

(** [default] is a record of [Null]s, i.e. keep the default, for all the socket options
    except that [nodelay] is [This true] to disable
    {{:https://en.wikipedia.org/wiki/Nagle%27s_algorithm} Nagle's algorithm}. *)
val default : t
