(** {1 Concrete types for IO} *)

include struct
  (** @closed *)

  (** {2 Types for IO in general} *)

  module Io_slice = Io_slice
  module Io_wait_flags = Io_wait_flags
  module Or_eof = Or_eof
  module Or_eof_or_would_block = Or_eof_or_would_block
  module Or_would_block = Or_would_block

  (** {2 Types for File Systems} *)

  module File_create = File_create

  (** {2 Types for Networking} *)

  module Socket_opts = Socket_opts
end
