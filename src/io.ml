(** {!Io} is a capability based library for direct-style I/O using the
    {{!Await_kernel.Await} [Await]} capability for blocking.

    See the {!section-architecture} and {!section-examples} sections below for more
    details. *)

(** {1 Interface} *)

include Io_kernel (** @inline *)

include Io_interfaces (** @inline *)

include Io_sys (** @inline *)

include Io_types (** @inline *)

(** {1:architecture Architecture}

    This library is the union of smaller interface libraries:

    {ul
     {- {!Io_kernel} defines the {!Io.t} capability, which represents a capability to
        perform system level IO such as accessing network interfaces and the file system.

       Basically, a function that has a signature of the form
       {@ocaml skip[
         val mystery : Io.t @ local -> (* ... *)
       ]}
       is a function that may perform (system level) IO, which includes the capability to
       suspend the current fiber for an indefinite amount of time to wait for an IO
       operation to complete.

       The {!Io.t} capability is intended to be potentially implementable in a scheduler
       dependent manner. For example, an implementation could be built to use [Async_unix]
       and to work only on the [Async] scheduler. Another implementation might use
       [io_uring] and work concurrently with any scheduler providing the
       {{!Await_kernel.Await} [Await.t]} or the [Concurrent.t] capability.

       Even though an {!Io.t} implementation itself is bound to a particular execution
       context, the operations provided by {!Io.t} implementations share a concrete
       {{!Io.Expert.Io_descr} [Io_descr.t]} or file or IO descriptor type. This allows
       portable higher-level IO interface implementations to be made that can be migrated
       between different {!Io.t} implementations. For example, a server might run on the
       [Async] scheduler and connections accepted by the server could then be spawned or
       forked as threads to be handled on another scheduler using a different {!Io.t}
       implementation.

       The operations provided by an {!Io.t} are all inside the {!Io.Expert} module and
       are only intended to be used in implementations of higher-level IO interfaces.
    }
     {- {!Io_interfaces} defines a family of abstract interfaces for IO such as

       - {!Readable} and {!Writable},
       - {!File} and {!Filesystem},
       - {!Listener}, {!Datagram_socket}, and {!Net}, i.e. networking.

       In other words, this library does not provide an IO implementation, but rather
       provides interfaces for programming against IO implementations, or more
       specifically, against a given {!Io.t} implementation.

       These interfaces are generally more specialized than e.g. operations in POSIX where
       various file types are all manipulated through file descriptors. The intention is
       that this should make it easier to use the interfaces correctly. For example, a
       {!Listener} is usually a socket, which, in POSIX, is a type of a file referred to
       by a file descriptor, and is configured to accept connections either locally or via
       a network interface.

       All of the interfaces are also intended to be implementable by expert users. For
       example, a special {{!Net} network} implementation could be provided that records
       all calls and another implementation that could playback such a recording for the
       purpose of writing tests. Another plausible example might be a {{!Filesystem} file
       system} implementation that accesses an archive, like a [zip] or [tar] file, rather
       than an OS file system.

       Basically all operations in these interfaces take an {!Io.t} as an argument.
       However, they are not required to use it. For example, a {!Readable} implementation
       might read from a string or bytes in memory and not use the {!Io.t} at all.
       Capabilities, like {!Io.t}, are fundamentally bound to the execution context of the
       code unlike "objects", like a {!Readable.t}, that you are manipulating and can
       usually move across contexts. Due to their binding to the execution context it is
       pretty much required that one keeps on passing an {!Io.t} through the program logic
       separately or as embedded within a richer capability to operations that potentially
       need it.
    }
     {- {!Io_sys} defines the {!Sys} module which gives access to the standard
        {{!Sys.stdin} input}, {{!Sys.stdout} output}, and {{!Sys.stderr} error} streams,
        system {{!Sys.cwd} file system} and {{!Sys.net} network interfaces}.

       In other words, {!Io_sys} implements the abstract interfaces for IO defined by
       {!Io_interfaces} and provides access to the system IO facilities through those
       interfaces.

       For typical programs, the {!Sys} module is the entry point to IO.
    }
     {- {!Io_types} defines concrete auxiliary types for IO interfaces.

       You will encounter these types as argument or result types of various IO operations
       and it is perhaps best to examine them in those contexts.
    }
    }

    To actually perform IO, you will need an implementation of the {!Io.t} capability.
    Such implementations are likely to be provided through or along with schedulers such
    as [Async] and other schedulers providing a [Concurrent.t] implementation, because an
    {!Io.t} implementation is fundamentally bound to the execution context and may also be
    specialized to work with or through a particular scheduler.

    {1:examples Examples}

    For the examples below, we first open a number of libraries:

    {[
      open Core
      open Ox
      open Io
    ]}

    {2 Read a file}

    Here is an example of a function that reads the contents of the specified file as a
    bigstring:

    {[
      let read_file_as_bigstring (io : Io.t @ local) (path : File_path.t @ local) =
        (let%with.tilde.stack file = Filesystem.open_file_to_read io Sys.cwd path in
         let file_size = I64.to_int_exn (File.size io file) in
         let bigstring = Bigstring.create file_size in
         let bytes_read =
           let%with.tilde.stack slice = Slice.with_bigstring bigstring in
           Isize.to_int_exn (File.read io file ~offset:I64.zero slice)
         in
         { aliased = Bigstring.sub ~pos:0 ~len:bytes_read bigstring })
          .aliased
      ;;
    ]} *)
