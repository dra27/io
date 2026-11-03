open Ox_kernel

type t = U8.t Slice.t
type read_only = (t Base.Modes.t[@modality read] [@kind (value_or_null & bits64) & word])

let[@inline] assert_non_empty buffer =
  if Slice.is_empty buffer then invalid_arg "buffer empty"
;;

module Ext = struct
  type t = U8.t Slice.Ext.t
  type read_only = (t Base.Modes.t[@modality read] [@kind bits64 & word])
end
