@@ portable

open Io_kernel

val%template use
  : ('t : k) ('r : value_or_null).
  close:(Io.t @ local -> 't @ local unique -> unit#) @ local once unyielding
  -> Io.t @ local
  -> 't @ local p unique
  -> f:('t @ local p -> 'r @ once unique) @ local once
  -> 'r @ once unique
[@@kind k = (value, value & value, (value & value) & value)]
[@@mode p = (nonportable, portable)]
