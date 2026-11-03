@@ portable

open Core
open Io_interfaces

(** Standard streams, system level file system and network.

    This module provides access to implementations of {{!Io_interfaces} IO interfaces}
    directly based on the {{!Io_kernel.Io} IO capability} implementation provided by an
    {{!Io_kernel.Io.t} [Io.t]}. *)

(** {1 Interface} *)

(** {2 Standard streams} *)

(** {{!Io_interfaces.Readable} Readable stream} for standard input. *)
val stdin : unit -> Readable.t

(** {{!Io_interfaces.Writable} Writable stream} for standard output. *)
val stdout : unit -> Writable.t

(** {{!Io_interfaces.Writable} Writable stream} for standard error. *)
val stderr : unit -> Writable.t

(** {2 File system} *)

(** Unrestricted system-level {{!Io_interfaces.Filesystem} file system}, relative to the
    current working directory. *)
val cwd : read_write Filesystem.t

(** {2 Network} *)

(** System-level {{!Io_interfaces.Net} network}. *)
val net : Net.t
