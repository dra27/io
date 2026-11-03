open Core
open Ox_kernel
open Io_kernel
open Io_interfaces
module Io_descr = Io_descr_native
module Io_result = Io.Expert.Io_result

module Readable_fd : Readable.Prim.S with type t = Io_descr.t = struct
  type t = Io_descr.t

  let close = Io.Expert.close_readable
  let read_or_cancel_exn = Io.Expert.read_stream_or_cancel_exn
  let is_nonblocking = Io.Expert.is_nonblocking
  let read_nonblocking = Io.Expert.read_stream_nonblocking
  let wait_or_cancel_exn = Io.Expert.wait_read_or_cancel_exn
end

module Writable_fd : Writable.Prim.S with type t = Io_descr.t = struct
  type t = Io_descr.t

  let close = Io.Expert.close_writable
  let write_or_cancel_exn = Io.Expert.write_stream_or_cancel_exn
  let is_nonblocking = Io.Expert.is_nonblocking
  let write_nonblocking = Io.Expert.write_stream_nonblocking
  let wait_or_cancel_exn = Io.Expert.wait_write_or_cancel_exn
end

module Listener_fd : Listener.Prim.S with type t = Io_descr.t = struct
  type t = Io_descr.t

  let close = Io.Expert.close_readable

  let accept_or_cancel_exn #(io, c, fd, ~opts) =
    let #(~fd, ~client_addr) = Io.Expert.accept_or_cancel_exn #(io, c, fd, ~opts) in
    let readable =
      Capsule.Owned.create (fun () ->
        (Readable.Expert.create [@mode portable]) fd (module Readable_fd))
    in
    let writable =
      Capsule.Owned.create (fun () ->
        (Writable.Expert.create [@mode portable]) fd (module Writable_fd))
    in
    let duplex = #{ Duplex.Owned.readable; writable } in
    #(~duplex, ~client_addr)
  ;;

  let addr = Io.Expert.addr
end

module Datagram_socket_fd : Datagram_socket.Prim.S with type t = Io_descr.t = struct
  type t = Io_descr.t

  let close = Io.Expert.close_bidirect
  let send_or_cancel_exn = Io.Expert.send_to_or_cancel_exn
  let send_nonblocking = Io.Expert.send_to_nonblocking
  let recv_or_cancel_exn = Io.Expert.recv_from_or_cancel_exn
  let recv_nonblocking = Io.Expert.recv_from_nonblocking
  let wait_or_cancel_exn = Io.Expert.wait_or_cancel_exn
  let addr = Io.Expert.addr
end

module Net_sys : Net.Prim.S with type t = unit = struct
  type t = unit

  let close #(_, _) =
    match
      invalid_arg "Net.close: closing this network implementation should be impossible"
    with
    | (_ : Nothing.t) -> .
  ;;

  let addr_info #(io, _, addr) = Io.Expert.addr_info #(io, addr)

  let stream_addrs #(io, _, ~host, ~service) =
    Io.Expert.stream_addrs #(io, ~host, ~service)
  ;;

  let connect_or_cancel_exn #(io, cancellation, _, ~opts, addr) =
    let fd = Io.Expert.connect_or_cancel_exn #(io, cancellation, ~opts, addr) in
    let readable =
      Capsule.Owned.create (fun () ->
        (Readable.Expert.create [@mode portable]) fd (module Readable_fd))
    in
    let writable =
      Capsule.Owned.create (fun () ->
        (Writable.Expert.create [@mode portable]) fd (module Writable_fd))
    in
    #{ Duplex.Owned.readable; writable }
  ;;

  let%template listen_or_cancel_exn
    #(io, cancellation, _, ~opts, ~max_pending_requests, addr)
    =
    let fd =
      Io.Expert.listen_or_cancel_exn
        #(io, cancellation, ~opts, ~max_pending_requests, addr)
    in
    (Listener.Expert.create [@mode p]) fd (module Listener_fd)
  [@@mode p = (nonportable, portable)]
  ;;

  let datagram_addrs #(io, _, ~host, ~service) =
    Io.Expert.datagram_addrs #(io, ~host, ~service)
  ;;

  let%template bind_datagram_socket_or_cancel_exn #(io, cancellation, _, ~opts, addr) =
    let fd =
      Io.Expert.bind_datagram_socket_or_cancel_exn
        #(io, cancellation, ~opts, ~bind_to:addr)
    in
    (Datagram_socket.Expert.create [@mode p]) fd (module Datagram_socket_fd)
  [@@mode p = (nonportable, portable)]
  ;;
end

type fd_with_offset =
  { fd : Io_descr.t @@ global
  ; mutable offset : i64
  }

module File_fd_as_readable : Readable.Prim.S with type t = fd_with_offset = struct
  type t = fd_with_offset

  let close #(io, { fd; _ }) = Io.Expert.close_readable #(io, fd)

  let read_or_cancel_exn #(io, cancellation, t, bs) =
    let n =
      Io.Expert.read_seekable_or_cancel_exn #(io, cancellation, t.fd, ~offset:t.offset, bs)
    in
    t.offset <- I64.( + ) t.offset (Isize.to_i64 n);
    Io_result.of_isize_non_negative_unsafe n
  ;;

  let is_nonblocking #(_io, _t) = false

  let read_nonblocking #(_io, _t, _bs) =
    match invalid_arg "Readable.read_nonblocking: files are blocking" with
    | (_ : Nothing.t) -> .
  ;;

  let wait_or_cancel_exn #(io, cancellation, { fd; _ }) =
    Io.Expert.wait_read_or_cancel_exn #(io, cancellation, fd)
  ;;
end

module File_fd_as_writable : Writable.Prim.S with type t = fd_with_offset = struct
  type t = fd_with_offset

  let close #(io, { fd; _ }) = Io.Expert.close_writable #(io, fd)

  let write_or_cancel_exn #(io, cancellation, t, bs) =
    let n =
      Io.Expert.write_seekable_or_cancel_exn
        #(io, cancellation, t.fd, ~offset:t.offset, bs)
    in
    t.offset <- I64.( + ) t.offset (Isize.to_i64 n);
    Io_result.of_isize_non_negative_unsafe n
  ;;

  let is_nonblocking #(_io, _t) = false

  let write_nonblocking #(_io, _t, _bs) =
    match invalid_arg "Writable.write_nonblocking: files are blocking" with
    | (_ : Nothing.t) -> .
  ;;

  let wait_or_cancel_exn #(io, cancellation, { fd; _ }) =
    Io.Expert.wait_write_or_cancel_exn #(io, cancellation, fd)
  ;;
end

module File_fd : File.Prim.S with type t = Io_descr.t = struct
  type t = Io_descr.t

  let size #(io, fd) =
    let stat = Io.Expert.stat #(io, fd) in
    stat.size
  ;;

  let%template readable #(_, fd, ~offset) =
    let fd = Io_descr.globalize fd in
    let readable =
      let fd = { fd; offset } in
      (Readable.Expert.create [@mode p]) fd (module File_fd_as_readable)
    in
    Io_descr.(incr Roles.readable) fd;
    readable
  [@@mode p = (nonportable, portable)]
  ;;

  let%template writable #(_, fd, ~offset) =
    let fd = Io_descr.globalize fd in
    let writable =
      let fd = { fd; offset } in
      (Writable.Expert.create [@mode p]) fd (module File_fd_as_writable)
    in
    Io_descr.(incr Roles.writable) fd;
    writable
  [@@mode p = (nonportable, portable)]
  ;;

  let close = Io.Expert.close_bidirect
  let resize = Io.Expert.resize
  let read_or_cancel_exn = Io.Expert.read_seekable_or_cancel_exn
  let write_or_cancel_exn = Io.Expert.write_seekable_or_cancel_exn
  let flush = Io.Expert.flush
end

module Filesystem_cwd : Filesystem.Prim.S with type t = unit = struct
  type t = unit

  let close #(_, _) =
    match
      invalid_arg "Filesystem.close: closing this file system should be impossible"
    with
    | (_ : Nothing.t) -> .
  ;;

  let%template stat #(io, _, path) =
    let cwd = Io.Expert.current_dir io in
    (Io.Expert.stat_at [@alloc a])
      #(io, ~relative_to:cwd, ~follow_link:true, path) [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let%template lstat #(io, _, path) =
    let cwd = Io.Expert.current_dir io in
    (Io.Expert.stat_at [@alloc a])
      #(io, ~relative_to:cwd, ~follow_link:false, path) [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let%template ls_dir #(io, _, path) =
    let cwd = Io.Expert.current_dir io in
    (Io.Expert.read_dir_at [@alloc a])
      #(io, ~relative_to:cwd, ~follow_link:true, path) [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let mkdir #(io, _, path, ~parents, perm) =
    let cwd = Io.Expert.current_dir io in
    Io.Expert.create_dir_at #(io, ~relative_to:cwd, path, ~parents, perm)
  ;;

  let rmdir #(io, _, path) =
    let cwd = Io.Expert.current_dir io in
    Io.Expert.remove_dir_at #(io, ~relative_to:cwd, path)
  ;;

  let rename #(io, _, ~src, ~dst) =
    let cwd = Io.Expert.current_dir io in
    Io.Expert.rename_at
      #(io, ~relative_to_before:cwd, ~before:src, ~relative_to_after:cwd, ~after:dst)
  ;;

  let unlink #(io, _, path) =
    let cwd = Io.Expert.current_dir io in
    Io.Expert.remove_file_at #(io, ~relative_to:cwd, path)
  ;;

  let symlink #(io, _, path, ~(referring_to : File_path.t)) =
    let cwd = Io.Expert.current_dir io in
    Io.Expert.create_link_at #(io, ~into:(referring_to :> string), ~relative_to:cwd, path)
  ;;

  let%template readlink #(io, _, path) =
    let cwd = Io.Expert.current_dir io in
    let path = Io.Expert.read_link_at #(io, ~relative_to:cwd, path) in
    File_path.of_string path
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let%template open_file_to_read #(io, _, path) =
    let cwd = Io.Expert.current_dir io in
    let fd =
      Io.Expert.open_file_to_read_at #(io, ~relative_to:cwd, ~follow_link:true, path)
    in
    (File.Expert.create [@mode p]) fd (module File_fd)
  [@@mode p = (nonportable, portable)]
  ;;

  let%template open_or_create_file #(io, _, ~create, path) =
    let cwd = Io.Expert.current_dir io in
    let fd =
      Io.Expert.open_or_create_file_at
        #(io, ~create, ~relative_to:cwd, ~follow_link:true, path)
    in
    (File.Expert.create [@mode p]) fd (module File_fd)
  [@@mode p = (nonportable, portable)]
  ;;

  let%template open_or_create_file_to_append #(io, _, ~create, path) =
    let cwd = Io.Expert.current_dir io in
    let fd =
      Io.Expert.open_or_create_file_to_append_at
        #(io, ~create, ~relative_to:cwd, ~follow_link:true, path)
    in
    (Writable.Expert.create [@mode p]) fd (module Writable_fd)
  [@@mode p = (nonportable, portable)]
  ;;
end

let stdin =
  (Readable.Expert.create [@mode portable])
    (Io_descr.(create Roles.readable) ~shutdown:false Core_unix.stdin)
    (module Readable_fd)
;;

let stdout =
  (Writable.Expert.create [@mode portable])
    (Io_descr.(create Roles.writable) ~shutdown:false Core_unix.stdout)
    (module Writable_fd)
;;

let stderr =
  (Writable.Expert.create [@mode portable])
    (Io_descr.(create Roles.writable) ~shutdown:false Core_unix.stderr)
    (module Writable_fd)
;;

let stdin () =
  Basement.Portability_hacks.magic_uncontended__promise_deeply_immutable__portable stdin
;;

let stdout () =
  Basement.Portability_hacks.magic_uncontended__promise_deeply_immutable__portable stdout
;;

let stderr () =
  Basement.Portability_hacks.magic_uncontended__promise_deeply_immutable__portable stderr
;;

let net = (Net.Expert.create [@mode portable]) () (module Net_sys)
let cwd = (Filesystem.Expert.create [@mode portable]) () (module Filesystem_cwd)
