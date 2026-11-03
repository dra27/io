(** {1 Abstract interfaces for IO} *)

(** {2 Interfaces for basic IO streams} *)

module Readable = Readable
module Writable = Writable
module Duplex = Duplex

(** {2 Interfaces for file systems} *)

module File = File
module Filesystem = Filesystem

(** {2 Interfaces for networking} *)

module Listener = Listener
module Datagram_socket = Datagram_socket
module Net = Net
