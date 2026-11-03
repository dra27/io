open Linux_ext
open Core
open Ox
open Await
open Io_kernel
open Io_types_private_native
open Filesystem_types_unix
module Io_descr = Io_descr_native
module Io_result = Io.Expert.Io_result
module Uring = Io_uring_bindings
module Urings = Io_uring_shards

module Io_prim : Io.Prim.S with type t = Urings.t = struct
  type t = Urings.t

  let close_raw_fd await t raw_fd =
    ignore
      (Urings.call_sq await t ~n:1 ~f:(fun #(sq, tag) ->
         Uring.Sq.push_close sq ~tag Uring.Sqe_flags.empty raw_fd)
       : Uring.Result.t)
  ;;

  let[@cold] close_raw_fd_and_reraise await t exn raw_fd =
    (* This is annotated as cold as this is in the unexpected error handling path and we
       want to minimize code size. *)
    let bt = Backtrace.Exn.most_recent () in
    close_raw_fd await t raw_fd;
    Exn.raise_with_original_backtrace exn bt
  ;;

  let set_datagram_opts
    raw_fd
    ({ Socket_opts.keep_alive
     ; permit_broadcast
     ; linger
     ; recv_buffer
     ; recv_timeout
     ; reuse_addr
     ; reuse_port
     ; send_buffer
     ; send_timeout
     ; nodelay = _
     }
      @ local)
    =
    (Or_null.iter [@mode local])
      keep_alive
      ~f:[%eta1 Caml_unix.setsockopt raw_fd SO_KEEPALIVE];
    (Or_null.iter [@mode local])
      permit_broadcast
      ~f:[%eta1 Caml_unix.setsockopt raw_fd SO_BROADCAST];
    (Or_null.iter [@mode local])
      linger
      ~f:[%eta1 Caml_unix.setsockopt_optint raw_fd SO_LINGER];
    (Or_null.iter [@mode local])
      recv_buffer
      ~f:[%eta1 Caml_unix.setsockopt_int raw_fd SO_RCVBUF];
    (Or_null.iter [@mode local])
      recv_timeout
      ~f:[%eta1 Caml_unix.setsockopt_float raw_fd SO_RCVTIMEO];
    (Or_null.iter [@mode local])
      reuse_addr
      ~f:[%eta1 Caml_unix.setsockopt raw_fd SO_REUSEADDR];
    (Or_null.iter [@mode local])
      reuse_port
      ~f:[%eta1 Caml_unix.setsockopt raw_fd SO_REUSEPORT];
    (Or_null.iter [@mode local])
      send_buffer
      ~f:[%eta1 Caml_unix.setsockopt_int raw_fd SO_SNDBUF];
    (Or_null.iter [@mode local])
      send_timeout
      ~f:[%eta1 Caml_unix.setsockopt_float raw_fd SO_SNDTIMEO];
    ()
  ;;

  let set_connected_opts raw_fd ({ Socket_opts.nodelay; _ } as opts) =
    set_datagram_opts raw_fd opts;
    (Or_null.iter [@mode local])
      nodelay
      ~f:[%eta1 Caml_unix.setsockopt raw_fd TCP_NODELAY];
    ()
  ;;

  (** Awaits until given [raw_fd] is available for [in_] or [out] operations as specified
      in [poll_mask] or until the await is terminated or canceled. *)
  let block_on_or_cancel_exn await cancel t raw_fd poll_mask =
    Urings.call_sq_non_neg_or_cancel_exn
      await
      cancel
      t
      ~n:1
      ~name:"poll_add"
      ~f:(fun #(sq, tag) ->
        Uring.Sq.push_poll_add sq ~tag Uring.Sqe_flags.empty raw_fd poll_mask)
    |> Epoll.Flags.of_int
  ;;

  let wait_or_cancel_exn #(await, cancel, t, fd, flags) =
    let raw_fd = Io_descr.unsafe_get fd in
    let mutable mask = Epoll.Flags.err in
    if Io_wait_flags.(do_intersect in_) flags then mask <- Epoll.Flags.(mask + in_);
    if Io_wait_flags.(do_intersect out) flags then mask <- Epoll.Flags.(mask + out);
    let events = block_on_or_cancel_exn await cancel t raw_fd mask in
    let mutable got = Io_wait_flags.of_int 0 in
    if Epoll.Flags.(do_intersect in_) events then got <- Io_wait_flags.(got + in_);
    if Epoll.Flags.(do_intersect out) events then got <- Io_wait_flags.(got + out);
    got
  ;;

  let close #(await, t, fd, role) =
    let actions = Io_descr.decr role fd in
    if not (Io_descr.Actions.is_empty actions)
    then (
      (* We need to shutdown and/or close the descriptor.

         We only submit at most a single shutdown command, combining both shutdowns if
         needed, and at most a single close command. We also submit both requests (i.e.
         shutdown and close) at once if possible. *)
      let raw_fd = Io_descr.unsafe_get fd in
      ignore
        (let shutdown_command = Io_descr.Actions.to_shutdown_command actions in
         let n =
           Bool.to_int (not (Or_null.is_null shutdown_command))
           + Bool.to_int (Io_descr.Actions.(do_intersect and_close) actions)
         in
         Urings.call_sq await t ~n ~f:(fun #(sq, tag) ->
           (* We use [io_hardlink] as we don't care about failures, i.e. if shutdown
              fails, we still want to close. We also don't raise an exception in case
              close fails. *)
           Or_null.iter shutdown_command ~f:(fun shutdown_command ->
             let sqe_flags =
               Bool.select
                 (Io_descr.Actions.(do_intersect and_close) actions)
                 Uring.Sqe_flags.io_hardlink
                 Uring.Sqe_flags.empty
             in
             Uring.Sq.push_shutdown sq ~tag sqe_flags raw_fd shutdown_command);
           if Io_descr.Actions.(do_intersect and_close) actions
           then Uring.Sq.push_close sq ~tag Uring.Sqe_flags.empty raw_fd)
         : Uring.Result.t);
      if (not (Io_descr.Actions.(do_intersect and_close) actions))
         && Io_descr.should_close_after actions fd
      then close_raw_fd await t raw_fd);
    #()
  ;;

  let get_and_raise_error raw_fd name =
    match Caml_unix.getsockopt_error raw_fd with
    | None -> ()
    | Some error -> raise (Caml_unix.Unix_error (error, name, ""))
  ;;

  let stream_addrs #(_await, _t, ~host, ~service) =
    Caml_unix.getaddrinfo host service [ AI_SOCKTYPE SOCK_STREAM ]
    |> List.map ~f:(fun addr_info -> Sockaddr_unix.of_unix_sockaddr_exn addr_info.ai_addr)
  ;;

  let call_socket await cancel t socket_domain socket_type =
    Urings.call_sq_fd_or_cancel_exn
      await
      cancel
      t
      ~n:1
      ~name:"socket"
      ~f:(fun #(sq, tag) ->
        Uring.Sq.push_socket
          sq
          ~tag
          Uring.Sqe_flags.empty
          socket_domain
          socket_type
          Uring.Socket_flags.(cloexec + nonblock)
          ~protocol:0)
    [@nontail]
  ;;

  let connect_or_cancel_exn #(await, cancel, t, ~opts, addr) =
    let raw_fd =
      call_socket await cancel t Uring.Socket_domain.inet Uring.Socket_type.stream
    in
    try
      let sockaddr_ref = Uring.Sockaddr_ref.for_sockaddr #() in
      let socklen_ref = Uring.Socklen_ref.empty #() in
      let #() = Uring.Sockaddr_ref.set_socklen sockaddr_ref socklen_ref in
      let #() = Uring.Sockaddr_ref.set_to_sockaddr sockaddr_ref addr in
      set_connected_opts raw_fd opts;
      Urings.call_sq_0_or_cancel_exn
        await
        cancel
        t
        ~n:1
        ~name:"connect"
        ~f:(fun #(sq, tag) ->
          let sockaddr_ref = Uring.Sockaddr_ref.unsafe_globalize sockaddr_ref in
          let socklen_ref = Uring.Socklen_ref.unsafe_globalize socklen_ref in
          Uring.Sq.push_connect
            sq
            ~tag
            Uring.Sqe_flags.empty
            raw_fd
            sockaddr_ref
            socklen_ref);
      Io_descr.(create Roles.(readable + writable)) ~shutdown:true raw_fd
    with
    | exn -> close_raw_fd_and_reraise await t exn raw_fd
  ;;

  let listen_or_cancel_exn #(await, cancel, t, ~opts, ~max_pending_requests, addr) =
    let raw_fd =
      call_socket await cancel t Uring.Socket_domain.inet Uring.Socket_type.stream
    in
    try
      let addr = Sockaddr_unix.to_unix_sockaddr addr in
      set_connected_opts raw_fd opts;
      (try Caml_unix.bind raw_fd addr with
       | Caml_unix.Unix_error ((EINPROGRESS | EINTR), _, _) ->
         let _ : Epoll.Flags.t =
           block_on_or_cancel_exn await cancel t raw_fd Epoll.Flags.out
         in
         get_and_raise_error raw_fd "bind");
      Caml_unix.listen raw_fd max_pending_requests;
      Io_descr.(create Roles.readable) ~shutdown:false raw_fd
    with
    | exn -> close_raw_fd_and_reraise await t exn raw_fd
  ;;

  let accept_or_cancel_exn #(await, cancel, t, fd, ~opts) =
    let raw_fd = Io_descr.unsafe_get fd in
    let sockaddr_ref = Uring.Sockaddr_ref.for_sockaddr #() in
    let socklen_ref = Uring.Socklen_ref.empty #() in
    let #() = Uring.Sockaddr_ref.set_socklen sockaddr_ref socklen_ref in
    let raw_fd =
      Urings.call_sq_fd_or_cancel_exn
        await
        cancel
        t
        ~n:1
        ~name:"accept"
        ~f:(fun #(sq, tag) ->
          let sockaddr_ref = Uring.Sockaddr_ref.unsafe_globalize sockaddr_ref in
          let socklen_ref = Uring.Socklen_ref.unsafe_globalize socklen_ref in
          Uring.Sq.push_accept
            sq
            ~tag
            Uring.Sqe_flags.empty
            raw_fd
            sockaddr_ref
            socklen_ref
            Uring.Socket_flags.(cloexec + nonblock))
    in
    (try set_connected_opts raw_fd opts with
     | exn -> close_raw_fd_and_reraise await t exn raw_fd);
    let client_addr = Uring.Sockaddr_ref.get_as_sockaddr sockaddr_ref socklen_ref in
    let fd = Io_descr.(create Roles.(readable + writable)) ~shutdown:true raw_fd in
    #(~fd, ~client_addr)
  ;;

  let datagram_addrs #(_await, _t, ~host, ~service) =
    Caml_unix.getaddrinfo host service [ AI_SOCKTYPE SOCK_DGRAM ]
    |> List.map ~f:(fun addr_info -> Sockaddr_unix.of_unix_sockaddr_exn addr_info.ai_addr)
  ;;

  let bind_datagram_socket_or_cancel_exn #(await, cancel, t, ~opts, ~bind_to) =
    let bind_to = Sockaddr_unix.to_unix_sockaddr bind_to in
    let raw_fd =
      call_socket await cancel t Uring.Socket_domain.inet Uring.Socket_type.dgram
    in
    try
      set_datagram_opts raw_fd opts;
      (try Caml_unix.bind raw_fd bind_to with
       | Caml_unix.Unix_error ((EINPROGRESS | EINTR), _, _) ->
         let _ : Epoll.Flags.t =
           block_on_or_cancel_exn await cancel t raw_fd Epoll.Flags.out
         in
         get_and_raise_error raw_fd "bind");
      Io_descr.(create Roles.(readable + writable)) ~shutdown:true raw_fd
    with
    | exn -> close_raw_fd_and_reraise await t exn raw_fd
  ;;

  let send_to_prim_or_cancel_exn await cancel t fd bs dst =
    let sockaddr = Uring.Sockaddr_ref.for_sockaddr #() in
    let #() = Uring.Sockaddr_ref.set_to_sockaddr sockaddr dst in
    let iovec =
      (Uring.Iovec.make [@mode read])
        [: { modal =
               (Buf.unsafe_globalize [@mode read])
                 ((Buf.magic_of_slice [@kind bits8] [@mode local read]) bs)
           }
        :]
    in
    let msghdr =
      Uring.Msghdr.(make [@mode read])
        (Uring.Sockaddr_ref.unsafe_globalize sockaddr)
        ((Uring.Iovec.unsafe_globalize [@mode read]) iovec)
    in
    let raw_fd = Io_descr.unsafe_get fd in
    Urings.call_sq_non_neg_or_cancel_exn
      await
      cancel
      t
      ~n:1
      ~name:"sendmsg"
      ~f:(fun #(sq, tag) ->
        let msghdr = (Uring.Msghdr.unsafe_globalize [@mode read]) msghdr in
        Uring.Sq.push_sendmsg
          sq
          ~tag
          Uring.Sqe_flags.empty
          raw_fd
          msghdr
          Uring.Send_flags.nosignal)
    |> Isize.of_int
  ;;

  let is_nonblocking #(_await, _t, fd) =
    let flags = Core_unix.fcntl_getfl (Io_descr.unsafe_get fd) in
    Core_unix.Open_flags.do_intersect flags Core_unix.Open_flags.nonblock
  ;;

  let send_to_nonblocking #(await, t, fd, bs, dst) =
    match
      let%with.tilde.stack ext_bs =
        (Io_slice.Expert.with_ext_out [@kind word]) (bs : Io_slice.read_only).modal
      in
      send_to_prim_or_cancel_exn await Cancellation.never t fd ext_bs dst [@nontail]
    with
    | n -> Io_result.of_isize_non_negative_unsafe n
    | exception Caml_unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _) ->
      Io_result.would_block
  ;;

  let send_to_or_cancel_exn #(await, cancel, t, fd, bs, dst) =
    let%with.tilde.stack bs =
      (Io_slice.Expert.with_ext_out [@kind word]) (bs : Io_slice.read_only).modal
    in
    let[@inline] rec loop () =
      match send_to_prim_or_cancel_exn await cancel t fd bs dst with
      | n -> n
      | exception Caml_unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _) ->
        let _ : Io_wait_flags.t =
          wait_or_cancel_exn #(await, cancel, t, fd, Io_wait_flags.out)
        in
        loop ()
    in
    loop () [@nontail]
  ;;

  let recv_from_prim_or_cancel_exn await cancel t fd bs =
    let raw_fd = Io_descr.unsafe_get fd in
    let sockaddr = Uring.Sockaddr_ref.for_sockaddr #() in
    let iovec =
      Uring.Iovec.make
        [: { modal =
               Buf.unsafe_globalize ((Buf.magic_of_slice [@kind bits8] [@mode local]) bs)
           }
        :]
    in
    let msghdr =
      Uring.Msghdr.make
        (Uring.Sockaddr_ref.unsafe_globalize sockaddr)
        (Uring.Iovec.unsafe_globalize iovec)
    in
    let len =
      Urings.call_sq_non_neg_or_cancel_exn
        await
        cancel
        t
        ~n:1
        ~name:"recvmsg"
        ~f:(fun #(sq, tag) ->
          let msghdr = Uring.Msghdr.unsafe_globalize msghdr in
          Uring.Sq.push_recvmsg
            sq
            ~tag
            Uring.Sqe_flags.empty
            raw_fd
            msghdr
            Uring.Recv_flags.empty)
    in
    let sender_addr =
      Uring.Sockaddr_ref.get_as_sockaddr sockaddr (Uring.Msghdr.socklen msghdr)
    in
    #(~len:(Isize.of_int len), ~sender_addr)
  ;;

  let recv_from_nonblocking #(await, t, fd, bs) =
    match
      let%with.tilde.stack bs =
        (Io_slice.Expert.with_ext_in [@kind word & value_or_null])
          bs
          ~len:(fun #(~len, ~sender_addr:_) -> len)
      in
      recv_from_prim_or_cancel_exn await Cancellation.never t fd bs [@nontail]
    with
    | #(~len, ~sender_addr) ->
      let len = Io_result.of_isize_non_negative_unsafe len in
      #(~len, ~sender_addr:(This sender_addr))
    | exception Caml_unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _) ->
      #(~len:Io_result.would_block, ~sender_addr:Null)
  ;;

  let recv_from_or_cancel_exn #(await, cancel, t, fd, bs) =
    let%with.tilde.stack bs =
      (Io_slice.Expert.with_ext_in [@kind word & value_or_null])
        bs
        ~len:(fun #(~len, ~sender_addr:_) -> len)
    in
    let[@inline] rec loop () =
      match recv_from_prim_or_cancel_exn await cancel t fd bs with
      | result -> result
      | exception Caml_unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _) ->
        let _ : Io_wait_flags.t =
          wait_or_cancel_exn #(await, cancel, t, fd, Io_wait_flags.in_)
        in
        loop ()
    in
    loop () [@nontail]
  ;;

  let addr_info #(_await, _t, addr) =
    let addr = Sockaddr_unix.to_unix_sockaddr addr in
    let info = Caml_unix.getnameinfo addr [] in
    #(~host:info.ni_hostname, ~service:info.ni_service)
  ;;

  let addr #(_await, _t, fd) =
    Sockaddr_unix.of_unix_sockaddr_exn (Caml_unix.getsockname (Io_descr.unsafe_get fd))
  ;;

  let read_prim_or_cancel_exn await cancel t fd offset bs =
    let bs = (Buf.magic_of_slice [@kind bits8] [@mode local]) bs in
    let raw_fd = Io_descr.unsafe_get fd in
    Urings.call_sq_non_neg_or_cancel_exn
      await
      cancel
      t
      ~n:1
      ~name:"read"
      ~f:(fun #(sq, tag) ->
        let bs = Buf.unsafe_globalize bs in
        Uring.Sq.push_read sq ~tag Uring.Sqe_flags.empty raw_fd ~offset bs)
    |> Isize.of_int
  ;;

  let read_seekable_or_cancel_exn #(await, cancel, t, fd, offset, bs) =
    let%with.tilde.stack bs =
      (Io_slice.Expert.with_ext_in [@kind word]) bs ~len:(fun x -> x)
    in
    read_prim_or_cancel_exn await cancel t fd offset bs [@nontail]
  ;;

  let write_prim_or_cancel_exn await cancel t fd offset bs =
    let bs = (Buf.magic_of_slice [@kind bits8] [@mode local read]) bs in
    let raw_fd = Io_descr.unsafe_get fd in
    Urings.call_sq_non_neg_or_cancel_exn
      await
      cancel
      t
      ~n:1
      ~name:"write"
      ~f:(fun #(sq, tag) ->
        let bs = (Buf.unsafe_globalize [@mode read]) bs in
        Uring.Sq.push_write sq ~tag Uring.Sqe_flags.empty raw_fd ~offset bs)
    |> Isize.of_int
  ;;

  let write_seekable_or_cancel_exn #(await, cancel, t, fd, offset, bs) =
    let%with.tilde.stack bs =
      (Io_slice.Expert.with_ext_out [@kind word]) (bs : Io_slice.read_only).modal
    in
    write_prim_or_cancel_exn await cancel t fd offset bs [@nontail]
  ;;

  let read_stream_or_cancel_exn #(await, cancel, t, fd, bs) =
    let len =
      let%with.tilde.stack bs =
        (Io_slice.Expert.with_ext_in [@kind word]) bs ~len:(fun x -> x)
      in
      let[@inline] rec loop () =
        match read_prim_or_cancel_exn await cancel t fd I64.zero bs with
        | n -> n
        | exception Caml_unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _) ->
          let _ : Io_wait_flags.t =
            wait_or_cancel_exn #(await, cancel, t, fd, Io_wait_flags.in_)
          in
          loop ()
      in
      loop () [@nontail]
    in
    Io_result.of_isize_non_negative_unsafe len
  ;;

  let write_stream_or_cancel_exn #(await, cancel, t, fd, bs) =
    let len =
      let%with.tilde.stack bs =
        (Io_slice.Expert.with_ext_out [@kind word]) (bs : Io_slice.read_only).modal
      in
      let[@inline] rec loop () =
        match write_prim_or_cancel_exn await cancel t fd I64.zero bs with
        | n -> n
        | exception Caml_unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _) ->
          let _ : Io_wait_flags.t =
            wait_or_cancel_exn #(await, cancel, t, fd, Io_wait_flags.out)
          in
          loop ()
      in
      loop () [@nontail]
    in
    Io_result.of_isize_non_negative_unsafe len
  ;;

  let read_stream_nonblocking #(await, t, fd, bs) =
    match
      let%with.tilde.stack bs =
        (Io_slice.Expert.with_ext_in [@kind word]) bs ~len:(fun x -> x)
      in
      read_prim_or_cancel_exn await Cancellation.never t fd I64.zero bs [@nontail]
    with
    | n -> Io_result.of_isize_non_negative_unsafe n
    | exception Caml_unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _) ->
      Io_result.would_block
  ;;

  let write_stream_nonblocking #(await, t, fd, bs) =
    match
      let%with.tilde.stack bs =
        (Io_slice.Expert.with_ext_out [@kind word]) (bs : Io_slice.read_only).modal
      in
      write_prim_or_cancel_exn await Cancellation.never t fd I64.zero bs [@nontail]
    with
    | n -> Io_result.of_isize_non_negative_unsafe n
    | exception Caml_unix.Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _) ->
      Io_result.would_block
  ;;

  let resize #(_await, _t, fd, (offset : I64.t)) =
    let raw_fd = Io_descr.unsafe_get fd in
    Caml_unix.LargeFile.ftruncate raw_fd (I64.box offset);
    #()
  ;;

  let flush #(await, t, fd) =
    let raw_fd = Io_descr.unsafe_get fd in
    Urings.call_sq_0_exn await t ~n:1 ~name:"fsync" ~f:(fun #(sq, tag) ->
      Uring.Sq.push_fsync sq ~tag Uring.Sqe_flags.empty raw_fd);
    #()
  ;;

  let%template stat #(_await, _t, fd) =
    let stats = Core_unix.fstat (Io_descr.unsafe_get fd) in
    File_stats.of_unix_stats stats
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let cwd =
    (* Aliased [Io_descr]s cannot be closed accidentally. *)
    Io_descr.(create Roles.readable) ~shutdown:false (Posixat.at_fdcwd ())
  ;;

  let current_dir #(_await, _t) = cwd

  let call_openat await t ~relative_to ~follow_link path mode flags perm =
    let flags = Uring.Open_flags.(flags + cloexec) in
    let flags = Bool.select follow_link flags Uring.Open_flags.(flags + nofollow) in
    let relative_to = Io_descr.unsafe_get relative_to in
    let path = Uring.String_ref.of_file_path path in
    Urings.call_sq_fd_exn await t ~n:1 ~name:"openat" ~f:(fun #(sq, tag) ->
      let path = Uring.String_ref.unsafe_globalize path in
      Uring.Sq.push_openat
        sq
        ~tag
        Uring.Sqe_flags.empty
        ~relative_to
        ~path
        mode
        flags
        perm)
    [@nontail]
  ;;

  let open_dir_at #(await, t, ~relative_to, ~follow_link, path) =
    let raw_fd =
      call_openat
        await
        t
        ~relative_to
        ~follow_link
        path
        Uring.Open_mode.rdonly
        Uring.Open_flags.directory
        File_permissions.empty
    in
    try Io_descr.(create Roles.readable) ~shutdown:false raw_fd with
    | exn -> close_raw_fd_and_reraise await t exn raw_fd
  ;;

  let%template read_dir_at #(await, t, ~relative_to, ~follow_link, path) =
    let raw_fd =
      call_openat
        await
        t
        ~relative_to
        ~follow_link
        path
        Uring.Open_mode.rdonly
        Uring.Open_flags.directory
        File_permissions.empty
    in
    match Posixat.fdopendir raw_fd with
    | dir_handle ->
      let mutable paths = [] in
      (try
         while true do
           match Caml_unix.readdir dir_handle with
           | "." | ".." -> ()
           | part -> paths <- File_path.Part.of_string part :: paths
         done
       with
       | End_of_file ->
         Caml_unix.closedir dir_handle;
         (Iarray.of_list [@alloc a]) paths [@exclave_if_stack a]
       | exn ->
         let bt = Backtrace.Exn.most_recent () in
         Caml_unix.closedir dir_handle;
         Exn.raise_with_original_backtrace exn bt)
    | exception exn -> close_raw_fd_and_reraise await t exn raw_fd
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let call_statx await t ~relative_to ~path at_flags statx_flags statx =
    Urings.call_sq await t ~n:1 ~f:(fun #(sq, tag) ->
      let path = Uring.String_ref.unsafe_globalize path in
      let statx = Uring.Statx_ref.unsafe_globalize statx in
      Uring.Sq.push_statx
        sq
        ~tag
        Uring.Sqe_flags.empty
        ~relative_to
        ~path
        at_flags
        statx_flags
        statx)
    [@nontail]
  ;;

  let%template stat_at #(await, t, ~relative_to, ~follow_link, path) =
    let relative_to = Io_descr.unsafe_get relative_to in
    let at_flags =
      Bool.select follow_link Uring.At_flags.empty Uring.At_flags.symlink_nofollow
    in
    (* We malloc the [statx] so that... *)
    (Uring.Statx_ref.with_malloc [@alloc a]) ~f:(stack_ fun statx ->
      let path = Uring.String_ref.of_file_path path in
      let result =
        call_statx await t ~relative_to ~path at_flags Uring.Statx_flags.basic_stats statx
      in
      Urings.on_error_raise_terminated_or_canceled_or_error await result ~name:"statx";
      (* ...the [File_stats] can be stack allocated without having the temporary [statx]
         also on the parent region. *)
      (Uring.Statx_ref.get [@alloc a]) statx [@exclave_if_stack a] [@nontail])
    [@exclave_if_stack a] [@nontail]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let call_mkdirat await t ~relative_to ~path perm =
    Urings.call_sq await t ~n:1 ~f:(fun #(sq, tag) ->
      let path = Uring.String_ref.unsafe_globalize path in
      Uring.Sq.push_mkdirat sq ~tag Uring.Sqe_flags.empty ~relative_to ~path perm)
    [@nontail]
  ;;

  let create_dir_at #(await, t, ~relative_to, path, ~parents, perm) =
    (* This logic is intended to match of [Core_unix.mkdir] and [Core_unix.mkdir_p]. *)
    let relative_to = Io_descr.unsafe_get relative_to in
    if parents
    then (
      let rec mkdir_idempotent path perm =
        let mkdir_result = call_mkdirat await t ~relative_to ~path perm in
        if Uring.Result.is_ok mkdir_result
        then mkdir_result
        else if Uring.Result.is_isdir mkdir_result
        then
          (* [mkdir] on MacOSX returns [EISDIR] if [dirname] exists and is a directory, in
             which case we can avoid the [statx] call below. *)
          Uring.Result.create_ok 0
        else (
          (* When mkdir fails, we need to check if the directory already exists. We cannot
             rely on [EEXISTS], because [EEXISTS] only implies that the name exists, not
             that it's a directory.

             We also cannot short-circuit if mkdir returns another error, because POSIX
             does not define any precedence ordering between errors and multiple errors
             may be applicable at the same time. For example, mkdir may return [EACCES] if
             the user doesn't have write permissions to the parent directory even if the
             child directory exists. *)
          let statx = Uring.Statx_ref.empty #() in
          let statx_result =
            call_statx
              await
              t
              ~relative_to
              ~path
              Uring.At_flags.empty
              Uring.Statx_flags.type_
              statx
          in
          if Uring.Result.is_ok statx_result
          then (
            match Uring.Statx_ref.get_kind statx with
            | Directory -> Uring.Result.create_ok 0
            | Block_device | Character_device | Fifo | Regular | Socket | Symlink ->
              (* [path] exists but is not a directory *)
              mkdir_result)
          else if Uring.Result.is_noent statx_result
          then
            if Uring.Result.is_exist mkdir_result
            then (
              (* We raced with another process that removed the directory or encountered a
                 broken symlink. *)
              let statx_result =
                call_statx
                  await
                  t
                  ~relative_to
                  ~path
                  Uring.At_flags.symlink_nofollow
                  Uring.Statx_flags.type_
                  statx
              in
              if Uring.Result.is_noent statx_result
              then mkdir_idempotent path perm
              else mkdir_result)
            else mkdir_result
          else mkdir_result)
      in
      let rec mkdir_p path_in perm =
        let path = Uring.String_ref.of_file_path path_in in
        let result = mkdir_idempotent path perm in
        if Uring.Result.is_error result
        then
          if Uring.Result.is_noent result
          then (
            match (File_path.dirname [@alloc stack]) path_in with
            | None ->
              Urings.raise_terminated_or_canceled_or_error await result ~name:"mkdirat"
            | Some dirname ->
              (* For intermediate directories, add owner write (0o200) and execute
                 permissions (0o100) so that we can traverse and create directories. *)
              mkdir_p dirname File_permissions.(perm lor o_w lor o_x);
              let result = mkdir_idempotent path perm in
              Urings.on_error_raise_terminated_or_canceled_or_error
                await
                result
                ~name:"mkdirat")
          else Urings.raise_terminated_or_canceled_or_error await result ~name:"mkdirat"
      in
      mkdir_p path perm)
    else (
      let result =
        let path = Uring.String_ref.of_file_path path in
        call_mkdirat await t ~relative_to ~path perm
      in
      Urings.on_error_raise_terminated_or_canceled_or_error await result ~name:"mkdirat");
    #()
  ;;

  let call_unlinkat await t ~relative_to path flags =
    let relative_to = Io_descr.unsafe_get relative_to in
    let path = Uring.String_ref.of_file_path path in
    Urings.call_sq_0_exn await t ~n:1 ~name:"unlinkat" ~f:(fun #(sq, tag) ->
      let path = Uring.String_ref.unsafe_globalize path in
      Uring.Sq.push_unlinkat sq ~tag Uring.Sqe_flags.empty ~relative_to ~path flags);
    #()
  ;;

  let remove_dir_at #(await, t, ~relative_to, path) =
    call_unlinkat await t ~relative_to path Uring.At_flags.removedir
  ;;

  let remove_file_at #(await, t, ~relative_to, path) =
    call_unlinkat await t ~relative_to path Uring.At_flags.empty
  ;;

  let rename_at #(await, t, ~relative_to_before, ~before, ~relative_to_after, ~after) =
    let relative_to_before = Io_descr.unsafe_get relative_to_before in
    let relative_to_after = Io_descr.unsafe_get relative_to_after in
    let before = Uring.String_ref.of_file_path before in
    let after = Uring.String_ref.of_file_path after in
    Urings.call_sq_0_exn await t ~n:1 ~name:"renameat" ~f:(fun #(sq, tag) ->
      let before = Uring.String_ref.unsafe_globalize before in
      let after = Uring.String_ref.unsafe_globalize after in
      Uring.Sq.push_renameat
        sq
        ~tag
        Uring.Sqe_flags.empty
        ~relative_to_before
        ~before
        ~relative_to_after
        ~after);
    #()
  ;;

  let create_link_at #(await, t, ~into, ~relative_to, path) =
    let relative_to = Io_descr.unsafe_get relative_to in
    let into = Uring.String_ref.of_string into in
    let path = Uring.String_ref.of_file_path path in
    Urings.call_sq_0_exn await t ~n:1 ~name:"symlinkat" ~f:(fun #(sq, tag) ->
      let into = Uring.String_ref.unsafe_globalize into in
      let path = Uring.String_ref.unsafe_globalize path in
      Uring.Sq.push_symlinkat sq ~tag Uring.Sqe_flags.empty ~into ~relative_to ~path);
    #()
  ;;

  let%template read_link_at #(_await, _t, ~relative_to, path) =
    let path = (path : File_path.t :> string) in
    let dir = Io_descr.unsafe_get relative_to in
    Posixat.readlinkat ~dir ~path
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let check_is_reg raw_fd =
    let stat = Caml_unix.fstat raw_fd in
    if not (phys_equal stat.st_kind S_REG)
    then raise (Caml_unix.Unix_error (EINVAL, "openat", "not a regular file"))
  ;;

  let open_file_to_read_at #(await, t, ~relative_to, ~follow_link, path) =
    let raw_fd =
      call_openat
        await
        t
        ~relative_to
        ~follow_link
        path
        Uring.Open_mode.rdonly
        Uring.Open_flags.empty
        File_permissions.empty
    in
    try
      check_is_reg raw_fd;
      Io_descr.(create Roles.(readable + writable)) ~shutdown:false raw_fd
    with
    | exn -> close_raw_fd_and_reraise await t exn raw_fd
  ;;

  let open_or_create_file_at #(await, t, ~create, ~relative_to, ~follow_link, path) =
    let perm = File_create.file_permissions create in
    let flags =
      match create with
      | Never -> Uring.Open_flags.empty
      | If_missing _ -> Uring.Open_flags.creat
      | Or_truncate _ -> Uring.Open_flags.(creat + trunc)
      | Always _ -> Uring.Open_flags.(creat + excl)
    in
    let raw_fd =
      call_openat await t ~relative_to ~follow_link path Uring.Open_mode.rdwr flags perm
    in
    try
      check_is_reg raw_fd;
      Io_descr.(create Roles.(readable + writable)) ~shutdown:false raw_fd
    with
    | exn -> close_raw_fd_and_reraise await t exn raw_fd
  ;;

  let open_or_create_file_to_append_at
    #(await, t, ~create, ~relative_to, ~follow_link, path)
    =
    let perm = File_create.file_permissions create in
    let flags =
      match create with
      | Never -> Uring.Open_flags.append
      | If_missing _ -> Uring.Open_flags.(append + creat)
      | Or_truncate _ -> Uring.Open_flags.(append + creat + trunc)
      | Always _ -> Uring.Open_flags.(append + creat + excl)
    in
    let raw_fd =
      call_openat await t ~relative_to ~follow_link path Uring.Open_mode.wronly flags perm
    in
    try
      check_is_reg raw_fd;
      Io_descr.(create Roles.writable) ~shutdown:false raw_fd
    with
    | exn -> close_raw_fd_and_reraise await t exn raw_fd
  ;;
end

let with_io await t ~f =
  let io = Io.Expert.create await (module Io_prim) t in
  f io [@nontail]
;;
