open Core
open Await_kernel
open Io_types
include Io_intf

type 'c inner_unpacked =
  #{ prim : 'c Prim.t @@ contended many portable
   ; context : 'c @@ many
   }

type inner = T : ('c : value mod contended portable). 'c inner_unpacked -> inner
[@@unboxed]

type t = (inner Capability.t[@kind value_or_null & value_or_null])

let await t = t.Capability.await

let with_terminator (t : t) terminator = exclave_
  { t with await = Await.with_terminator t.await terminator }
;;

module Expert = struct
  module Io_result = Io_result

  (* *)

  let create await prim context : t = exclave_
    { await; capability = T #{ prim; context } }
  ;;

  (* *)

  let close #(({ await; capability = T #{ prim = (module Prim); context } } : t), fd, role)
    =
    Prim.close #(await, context, fd, role)
  ;;

  let close_readable #(io, fd) = close #(io, fd, Io_descr.Roles.(readable))
  let close_writable #(io, fd) = close #(io, fd, Io_descr.Roles.(writable))
  let close_bidirect #(io, fd) = close #(io, fd, Io_descr.Roles.(readable + writable))

  (* Stream sockets *)

  let stream_addrs
    #(({ await; capability = T #{ prim = (module Prim); context } } : t), ~host, ~service)
    =
    Prim.stream_addrs #(await, context, ~host, ~service)
  ;;

  let connect_or_cancel_exn
    #( ({ await; capability = T #{ prim = (module Prim); context } } : t)
     , cancellation
     , ~opts
     , addr )
    =
    Prim.connect_or_cancel_exn #(await, cancellation, context, ~opts, addr)
  ;;

  let listen_or_cancel_exn #(io, cancellation, ~opts, ~max_pending_requests, addr) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.listen_or_cancel_exn
      #(await, cancellation, context, ~opts, ~max_pending_requests, addr)
  ;;

  let accept_or_cancel_exn
    #( ({ await; capability = T #{ prim = (module Prim); context } } : t)
     , cancellation
     , fd
     , ~opts )
    =
    Prim.accept_or_cancel_exn #(await, cancellation, context, fd, ~opts)
  ;;

  (* Datagram sockets *)

  let datagram_addrs
    #(({ await; capability = T #{ prim = (module Prim); context } } : t), ~host, ~service)
    =
    Prim.datagram_addrs #(await, context, ~host, ~service)
  ;;

  let bind_datagram_socket_or_cancel_exn #(io, cancellation, ~opts, ~bind_to) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.bind_datagram_socket_or_cancel_exn
      #(await, cancellation, context, ~opts, ~bind_to)
  ;;

  let recv_from_or_cancel_exn
    #( ({ await; capability = T #{ prim = (module Prim); context } } : t)
     , cancellation
     , fd
     , buf )
    =
    (* Allows zero length operations. *)
    Prim.recv_from_or_cancel_exn #(await, cancellation, context, fd, buf)
  ;;

  let is_nonblocking
    #(({ await; capability = T #{ prim = (module Prim); context } } : t), fd)
    =
    Prim.is_nonblocking #(await, context, fd)
  ;;

  let recv_from_nonblocking
    #(({ await; capability = T #{ prim = (module Prim); context } } : t), fd, buf)
    =
    (* Allows zero length operations. *)
    Prim.recv_from_nonblocking #(await, context, fd, buf)
  ;;

  let send_to_or_cancel_exn
    #( ({ await; capability = T #{ prim = (module Prim); context } } : t)
     , cancellation
     , fd
     , buf
     , addr )
    =
    (* Allows zero length operations. *)
    Prim.send_to_or_cancel_exn #(await, cancellation, context, fd, buf, addr)
  ;;

  let send_to_nonblocking
    #(({ await; capability = T #{ prim = (module Prim); context } } : t), fd, buf, addr)
    =
    (* Allows zero length operations. *)
    Prim.send_to_nonblocking #(await, context, fd, buf, addr)
  ;;

  (* Net *)

  let addr_info #(({ await; capability = T #{ prim = (module Prim); context } } : t), addr)
    =
    Prim.addr_info #(await, context, addr)
  ;;

  let addr #(({ await; capability = T #{ prim = (module Prim); context } } : t), fd) =
    Prim.addr #(await, context, fd)
  ;;

  (* Streams *)

  let read_stream_or_cancel_exn
    #( ({ await; capability = T #{ prim = (module Prim); context } } : t)
     , cancellation
     , fd
     , buf )
    =
    Io_slice.assert_non_empty buf;
    Prim.read_stream_or_cancel_exn #(await, cancellation, context, fd, buf)
  ;;

  let read_stream_nonblocking
    #(({ await; capability = T #{ prim = (module Prim); context } } : t), fd, buf)
    =
    Io_slice.assert_non_empty buf;
    Prim.read_stream_nonblocking #(await, context, fd, buf)
  ;;

  let write_stream_or_cancel_exn
    #( ({ await; capability = T #{ prim = (module Prim); context } } : t)
     , cancellation
     , fd
     , buf )
    =
    Io_slice.assert_non_empty (buf : Io_slice.read_only).modal;
    Prim.write_stream_or_cancel_exn #(await, cancellation, context, fd, buf)
  ;;

  let write_stream_nonblocking
    #(({ await; capability = T #{ prim = (module Prim); context } } : t), fd, buf)
    =
    Io_slice.assert_non_empty (buf : Io_slice.read_only).modal;
    Prim.write_stream_nonblocking #(await, context, fd, buf)
  ;;

  let wait_or_cancel_exn
    #( ({ await; capability = T #{ prim = (module Prim); context } } : t)
     , cancellation
     , fd
     , flags )
    =
    Prim.wait_or_cancel_exn #(await, cancellation, context, fd, flags)
  ;;

  let wait_read_or_cancel_exn #(io, cancellation, fd) =
    ignore
      (wait_or_cancel_exn #(io, cancellation, fd, Io_wait_flags.in_) : Io_wait_flags.t)
  ;;

  let wait_write_or_cancel_exn #(io, cancellation, fd) =
    ignore
      (wait_or_cancel_exn #(io, cancellation, fd, Io_wait_flags.out) : Io_wait_flags.t)
  ;;

  (* Seekables *)

  let read_seekable_or_cancel_exn #(io, cancellation, fd, ~offset, buf) =
    (* Allows zero length operations. *)
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.read_seekable_or_cancel_exn #(await, cancellation, context, fd, offset, buf)
  ;;

  let write_seekable_or_cancel_exn #(io, cancellation, fd, ~offset, buf) =
    (* Allows zero length operations. *)
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.write_seekable_or_cancel_exn #(await, cancellation, context, fd, offset, buf)
  ;;

  let resize
    #(({ await; capability = T #{ prim = (module Prim); context } } : t), fd, offset)
    =
    Prim.resize #(await, context, fd, offset)
  ;;

  let flush #(({ await; capability = T #{ prim = (module Prim); context } } : t), fd) =
    Prim.flush #(await, context, fd)
  ;;

  let%template stat
    #(({ await; capability = T #{ prim = (module Prim); context } } : t), fd)
    =
    (Prim.stat [@alloc a]) #(await, context, fd) [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  (* File system *)

  let current_dir ({ await; capability = T #{ prim = (module Prim); context } } : t) =
    Prim.current_dir #(await, context)
  ;;

  let open_dir_at #(io, ~relative_to, ~follow_link, path) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.open_dir_at #(await, context, ~relative_to, ~follow_link, path)
  ;;

  let%template read_dir_at #(io, ~relative_to, ~follow_link, path) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    (Prim.read_dir_at [@alloc a])
      #(await, context, ~relative_to, ~follow_link, path) [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let%template stat_at #(io, ~relative_to, ~follow_link, path) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    (Prim.stat_at [@alloc a])
      #(await, context, ~relative_to, ~follow_link, path) [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let create_dir_at
    #( ({ await; capability = T #{ prim = (module Prim); context } } : t)
     , ~relative_to
     , path
     , ~parents
     , perm )
    =
    Prim.create_dir_at #(await, context, ~relative_to, path, ~parents, perm)
  ;;

  let remove_dir_at #(io, ~relative_to, path) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.remove_dir_at #(await, context, ~relative_to, path)
  ;;

  let remove_file_at #(io, ~relative_to, path) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.remove_file_at #(await, context, ~relative_to, path)
  ;;

  let rename_at #(io, ~relative_to_before, ~before, ~relative_to_after, ~after) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.rename_at
      #(await, context, ~relative_to_before, ~before, ~relative_to_after, ~after)
  ;;

  let create_link_at #(io, ~into, ~relative_to, path) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.create_link_at #(await, context, ~into, ~relative_to, path)
  ;;

  let%template read_link_at
    #( ({ await; capability = T #{ prim = (module Prim); context } } : t)
     , ~relative_to
     , path )
    =
    (Prim.read_link_at [@alloc a])
      #(await, context, ~relative_to, path) [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let open_file_to_read_at #(io, ~relative_to, ~follow_link, path) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.open_file_to_read_at #(await, context, ~relative_to, ~follow_link, path)
  ;;

  let open_or_create_file_at #(io, ~create, ~relative_to, ~follow_link, path) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.open_or_create_file_at
      #(await, context, ~create, ~relative_to, ~follow_link, path)
  ;;

  let open_or_create_file_to_append_at #(io, ~create, ~relative_to, ~follow_link, path) =
    let ({ await; capability = T #{ prim = (module Prim); context } } : t) = io in
    Prim.open_or_create_file_to_append_at
      #(await, context, ~create, ~relative_to, ~follow_link, path)
  ;;
end
