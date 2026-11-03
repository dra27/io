@@ portable

open Ox

include module type of struct
  include Io_types
end

module Io_slice : sig
  include module type of struct
    include Io_types.Io_slice
  end

  module Expert : sig
    (** [with_ext_in t ~len ~f] calls [f] either with [t] cast to an external slice, when
        possible, or with a new external slice of same length. After [f] returns [result],
        in case a new external slice was allocated, the first [len result] bytes of the
        external slice are copied to the first [len result] bytes of slice [t].

        [len result] must return a value that is less than the size of the slice *)
    val%template with_ext_in
      : ('a : k).
      t @ local
      -> len:('a @ local -> isize) @ local once
      -> f:(Ext.t @ local -> 'a) @ local once
      -> 'a
    [@@kind k = (word, word & value_or_null)]

    (** [with_ext_out t ~f] calls [f] either with [t] cast to an external slice, when
        possible, or with a new external slice of same length and contents copied from
        slice [t]. *)
    val%template with_ext_out
      : ('a : k).
      t @ local read -> f:(Ext.t @ local read -> 'a) @ local once -> 'a
    [@@kind k = word]
  end
end
