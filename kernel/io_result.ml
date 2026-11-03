open Ox_kernel
open Io_types

type t = isize

let would_block = Isize.of_int (-1)
let eof = Isize.zero

let%template[@inline] to_or_eof result : (_ Or_eof.t[@kind word]) =
  if Isize.O.(result <= eof) then Eof else Ok result [@exclave_if_local l]
[@@mode l = (global, local)]
;;

let%template to_or_eof_or_would_block result =
  if Isize.O.(result < eof)
  then (Would_block : (_ Or_eof_or_would_block.t[@kind word]))
  else if Isize.O.(result = eof)
  then Eof
  else Ok result [@exclave_if_local l]
[@@mode l = (global, local)]
;;

let%template to_or_would_block result =
  if Isize.O.(result < eof)
  then (Would_block : (_ Or_would_block.t[@kind word]))
  else Ok result [@exclave_if_local l]
[@@mode l = (global, local)]
;;

let[@inline] to_isize_non_negative_unsafe x = x
let[@inline] of_isize_non_negative_unsafe x = x
let of_int_non_negative_unsafe = Isize.of_int
