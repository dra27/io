@@ portable

open Ox_kernel
open Io_types

type t = private isize

val would_block : t
val eof : t

val%template to_or_eof : t -> (isize Or_eof.t[@kind word]) @ l
[@@mode l = (global, local)]

val%template to_or_eof_or_would_block
  :  t
  -> (isize Or_eof_or_would_block.t[@kind word]) @ l
[@@mode l = (global, local)]

val%template to_or_would_block : t -> (isize Or_would_block.t[@kind word]) @ l
[@@mode l = (global, local)]

val to_isize_non_negative_unsafe : t -> isize
val of_isize_non_negative_unsafe : isize -> t
val of_int_non_negative_unsafe : int -> t
