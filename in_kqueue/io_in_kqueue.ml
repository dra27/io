open Core
open Ox
open Io_kernel
open Io_types_private_native
open Filesystem_types_unix
module Io_descr = Io_descr_native
module Io_result = Io.Expert.Io_result
module Kqueue_poller = Kqueue_poller
module Poller = Kqueue_poller
module Filters = Poller.Filters
module Syscall = Poller.Syscall

type t = Poller.t

module Raw = struct
  (* Stream and seekable reads and writes. These return a non-negative result or a
     negated errno and may release the runtime lock (the buffers are external). *)

  external read_stream
    :  (int[@untagged])
    -> Mem.t @ local
    -> Isize.t
    -> (int[@untagged])
    @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_read_stream"

  external write_stream
    :  (int[@untagged])
    -> Mem.t @ local read
    -> Isize.t
    -> (int[@untagged])
    @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_write_stream"

  external pread
    :  (int[@untagged])
    -> Mem.t @ local
    -> Isize.t
    -> I64.t
    -> (int[@untagged])
    @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_pread"

  external pwrite
    :  (int[@untagged])
    -> Mem.t @ local read
    -> Isize.t
    -> I64.t
    -> (int[@untagged])
    @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_pwrite"

  (* Datagram operations raise [Unix_error] on failure. *)

  external sendto
    :  Core_unix.File_descr.t
    -> Mem.t @ local read
    -> Isize.t
    -> Core_unix.sockaddr
    -> int
    @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_sendto"

  external recvfrom
    :  Core_unix.File_descr.t
    -> Mem.t @ local
    -> Isize.t
    -> int * Core_unix.sockaddr
    @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_recvfrom"
end

module Io_prim : Io.Prim.S with type t = Poller.t = struct
  type t = Poller.t

  let close_raw_fd raw_fd =
    (* Like the [io_uring] implementation, we don't raise in case [close] fails. *)
    try Caml_unix.close raw_fd with
    | Caml_unix.Unix_error _ -> ()
  ;;

  let[@cold] close_raw_fd_and_reraise exn raw_fd =
    let bt = Backtrace.Exn.most_recent () in
    close_raw_fd raw_fd;
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

  let check name result = if result < 0 then Syscall.raise_unix_error (-result) ~name

  (** Makes a socket created by us non-blocking and suppresses [SIGPIPE] for it, as
      macOS has no [MSG_NOSIGNAL]. *)
  let prepare_socket raw_fd =
    check "fcntl" (Syscall.set_nonblock raw_fd);
    check "setsockopt" (Syscall.set_nosigpipe raw_fd)
  ;;

  let create_socket socket_domain socket_type =
    let raw_fd = Caml_unix.socket ~cloexec:true socket_domain socket_type 0 in
    (try prepare_socket raw_fd with
     | exn -> close_raw_fd_and_reraise exn raw_fd);
    raw_fd
  ;;

  (** Awaits until [raw_fd] is ready for the given [filters]. Used after an operation
      returned [EAGAIN], which means [raw_fd] is not a regular file, so there is no need
      to probe readiness first. *)
  let block_on_or_cancel_exn await cancel t raw_fd filters =
    ignore (Poller.wait_or_cancel_exn await cancel t raw_fd filters : Filters.t)
  ;;

  let wait_or_cancel_exn #(await, cancel, t, fd, flags) =
    let raw_fd = Io_descr.unsafe_get fd in
    let mutable filters = 0 in
    if Io_wait_flags.(do_intersect in_) flags then filters <- filters lor Filters.in_;
    if Io_wait_flags.(do_intersect out) flags then filters <- filters lor Filters.out;
    if filters = 0
    then Io_wait_flags.of_int 0
    else (
      (* Probe first: this avoids registering a knote when the descriptor is already
         ready, and kqueue would never report a regular file at end-of-file as readable,
         whereas [poll] reports regular files as always ready. *)
      let ready =
        let ready = Poller.poll raw_fd filters in
        if ready land Filters.in_out <> 0
        then ready
        else Poller.wait_or_cancel_exn await cancel t raw_fd filters
      in
      let mutable got = Io_wait_flags.of_int 0 in
      if Filters.mem Filters.in_ ready then got <- Io_wait_flags.(got + in_);
      if Filters.mem Filters.out ready then got <- Io_wait_flags.(got + out);
      got)
  ;;

  let close #(_await, _t, fd, role) =
    let actions = Io_descr.decr role fd in
    if not (Io_descr.Actions.is_empty actions)
    then (
      let raw_fd = Io_descr.unsafe_get fd in
      (match Io_descr.Actions.to_shutdown_command actions with
       | Null -> ()
       | This shutdown_command ->
         (* As with the [io_uring] implementation, we don't care about failures here: if
            shutdown fails, we still want to close. *)
         (try Caml_unix.shutdown raw_fd shutdown_command with
          | Caml_unix.Unix_error _ -> ()));
      if Io_descr.Actions.(do_intersect and_close) actions
         || Io_descr.should_close_after actions fd
      then close_raw_fd raw_fd);
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

  let connect_or_cancel_exn #(await, cancel, t, ~opts, addr) =
    let addr = Sockaddr_unix.to_unix_sockaddr addr in
    let raw_fd = create_socket (Caml_unix.domain_of_sockaddr addr) SOCK_STREAM in
    try
      set_connected_opts raw_fd opts;
      (try Caml_unix.connect raw_fd addr with
       | Caml_unix.Unix_error ((EINPROGRESS | EINTR), _, _) ->
         block_on_or_cancel_exn await cancel t raw_fd Filters.out;
         get_and_raise_error raw_fd "connect");
      Io_descr.(create Roles.(readable + writable)) ~shutdown:true raw_fd
    with
    | exn -> close_raw_fd_and_reraise exn raw_fd
  ;;

  let listen_or_cancel_exn #(await, cancel, t, ~opts, ~max_pending_requests, addr) =
    let addr = Sockaddr_unix.to_unix_sockaddr addr in
    let raw_fd = create_socket (Caml_unix.domain_of_sockaddr addr) SOCK_STREAM in
    try
      set_connected_opts raw_fd opts;
      (try Caml_unix.bind raw_fd addr with
       | Caml_unix.Unix_error ((EINPROGRESS | EINTR), _, _) ->
         block_on_or_cancel_exn await cancel t raw_fd Filters.out;
         get_and_raise_error raw_fd "bind");
      Caml_unix.listen raw_fd max_pending_requests;
      Io_descr.(create Roles.readable) ~shutdown:false raw_fd
    with
    | exn -> close_raw_fd_and_reraise exn raw_fd
  ;;

  let accept_or_cancel_exn #(await, cancel, t, fd, ~opts) =
    let listen_fd = Io_descr.unsafe_get fd in
    let rec loop () =
      match Caml_unix.accept ~cloexec:true listen_fd with
      | result -> result
      | exception Caml_unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
        block_on_or_cancel_exn await cancel t listen_fd Filters.in_;
        loop ()
      | exception Caml_unix.Unix_error ((EINTR | ECONNABORTED), _, _) -> loop ()
    in
    let raw_fd, client_addr = loop () in
    (try
       prepare_socket raw_fd;
       set_connected_opts raw_fd opts
     with
     | exn -> close_raw_fd_and_reraise exn raw_fd);
    let client_addr = Sockaddr_unix.of_unix_sockaddr_exn client_addr in
    let fd = Io_descr.(create Roles.(readable + writable)) ~shutdown:true raw_fd in
    #(~fd, ~client_addr)
  ;;

  let datagram_addrs #(_await, _t, ~host, ~service) =
    Caml_unix.getaddrinfo host service [ AI_SOCKTYPE SOCK_DGRAM ]
    |> List.map ~f:(fun addr_info -> Sockaddr_unix.of_unix_sockaddr_exn addr_info.ai_addr)
  ;;

  let bind_datagram_socket_or_cancel_exn #(await, cancel, t, ~opts, ~bind_to) =
    let bind_to = Sockaddr_unix.to_unix_sockaddr bind_to in
    let raw_fd = create_socket (Caml_unix.domain_of_sockaddr bind_to) SOCK_DGRAM in
    try
      set_datagram_opts raw_fd opts;
      (try Caml_unix.bind raw_fd bind_to with
       | Caml_unix.Unix_error ((EINPROGRESS | EINTR), _, _) ->
         block_on_or_cancel_exn await cancel t raw_fd Filters.out;
         get_and_raise_error raw_fd "bind");
      Io_descr.(create Roles.(readable + writable)) ~shutdown:true raw_fd
    with
    | exn -> close_raw_fd_and_reraise exn raw_fd
  ;;

  let send_to_prim fd bs dst =
    let #{ mem; len } : Buf.t =
      (Buf.magic_of_slice [@kind bits8] [@mode local read]) bs
    in
    Raw.sendto (Io_descr.unsafe_get fd) mem len (Sockaddr_unix.to_unix_sockaddr dst)
    |> Isize.of_int
  ;;

  let is_nonblocking #(_await, _t, fd) =
    let flags = Core_unix.fcntl_getfl (Io_descr.unsafe_get fd) in
    Core_unix.Open_flags.do_intersect flags Core_unix.Open_flags.nonblock
  ;;

  let send_to_nonblocking #(_await, _t, fd, bs, dst) =
    match
      let%with.tilde.stack ext_bs =
        (Io_slice.Expert.with_ext_out [@kind word]) (bs : Io_slice.read_only).modal
      in
      send_to_prim fd ext_bs dst [@nontail]
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
      match send_to_prim fd bs dst with
      | n -> n
      | exception Caml_unix.Unix_error (EINTR, _, _) -> loop ()
      | exception Caml_unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
        block_on_or_cancel_exn await cancel t (Io_descr.unsafe_get fd) Filters.out;
        loop ()
    in
    loop () [@nontail]
  ;;

  let recv_from_prim fd bs =
    let #{ mem; len } : Buf.t = (Buf.magic_of_slice [@kind bits8] [@mode local]) bs in
    let len, sender_addr = Raw.recvfrom (Io_descr.unsafe_get fd) mem len in
    #(~len:(Isize.of_int len), ~sender_addr:(Sockaddr_unix.of_unix_sockaddr_exn sender_addr))
  ;;

  let recv_from_nonblocking #(_await, _t, fd, bs) =
    match
      let%with.tilde.stack bs =
        (Io_slice.Expert.with_ext_in [@kind word & value_or_null])
          bs
          ~len:(fun #(~len, ~sender_addr:_) -> len)
      in
      recv_from_prim fd bs [@nontail]
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
      match recv_from_prim fd bs with
      | result -> result
      | exception Caml_unix.Unix_error (EINTR, _, _) -> loop ()
      | exception Caml_unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
        block_on_or_cancel_exn await cancel t (Io_descr.unsafe_get fd) Filters.in_;
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

  (* Streams *)

  let read_stream_raw raw_fd bs =
    let #{ mem; len } : Buf.t = (Buf.magic_of_slice [@kind bits8] [@mode local]) bs in
    Raw.read_stream (Core_unix.File_descr.to_int raw_fd) mem len
  ;;

  let write_stream_raw raw_fd bs =
    let #{ mem; len } : Buf.t =
      (Buf.magic_of_slice [@kind bits8] [@mode local read]) bs
    in
    Raw.write_stream (Core_unix.File_descr.to_int raw_fd) mem len
  ;;

  let read_stream_or_cancel_exn #(await, cancel, t, fd, bs) =
    let raw_fd = Io_descr.unsafe_get fd in
    let len =
      let%with.tilde.stack bs =
        (Io_slice.Expert.with_ext_in [@kind word]) bs ~len:(fun x -> x)
      in
      let[@inline] rec loop () =
        let n = read_stream_raw raw_fd bs in
        if n >= 0
        then Isize.of_int n
        else if n = -Syscall.eagain
        then (
          block_on_or_cancel_exn await cancel t raw_fd Filters.in_;
          loop ())
        else if n = -Syscall.eintr
        then loop ()
        else (
          match Syscall.raise_unix_error (-n) ~name:"read" with
          | (_ : Nothing.t) -> .)
      in
      loop () [@nontail]
    in
    Io_result.of_isize_non_negative_unsafe len
  ;;

  let write_stream_or_cancel_exn #(await, cancel, t, fd, bs) =
    let raw_fd = Io_descr.unsafe_get fd in
    let len =
      let%with.tilde.stack bs =
        (Io_slice.Expert.with_ext_out [@kind word]) (bs : Io_slice.read_only).modal
      in
      let[@inline] rec loop () =
        let n = write_stream_raw raw_fd bs in
        if n >= 0
        then Isize.of_int n
        else if n = -Syscall.eagain
        then (
          block_on_or_cancel_exn await cancel t raw_fd Filters.out;
          loop ())
        else if n = -Syscall.eintr
        then loop ()
        else (
          match Syscall.raise_unix_error (-n) ~name:"write" with
          | (_ : Nothing.t) -> .)
      in
      loop () [@nontail]
    in
    Io_result.of_isize_non_negative_unsafe len
  ;;

  let read_stream_nonblocking #(_await, _t, fd, bs) =
    let raw_fd = Io_descr.unsafe_get fd in
    let n =
      let%with.tilde.stack bs =
        (Io_slice.Expert.with_ext_in [@kind word]) bs ~len:(fun n ->
          Isize.of_int (Int.max (Isize.to_int_trunc n) 0))
      in
      Isize.of_int (read_stream_raw raw_fd bs)
    in
    let n = Isize.to_int_trunc n in
    if n >= 0
    then Io_result.of_int_non_negative_unsafe n
    else if n = -Syscall.eagain || n = -Syscall.eintr
    then Io_result.would_block
    else (
      match Syscall.raise_unix_error (-n) ~name:"read" with
      | (_ : Nothing.t) -> .)
  ;;

  let write_stream_nonblocking #(_await, _t, fd, bs) =
    let raw_fd = Io_descr.unsafe_get fd in
    let n =
      let%with.tilde.stack bs =
        (Io_slice.Expert.with_ext_out [@kind word]) (bs : Io_slice.read_only).modal
      in
      Isize.of_int (write_stream_raw raw_fd bs)
    in
    let n = Isize.to_int_trunc n in
    if n >= 0
    then Io_result.of_int_non_negative_unsafe n
    else if n = -Syscall.eagain || n = -Syscall.eintr
    then Io_result.would_block
    else (
      match Syscall.raise_unix_error (-n) ~name:"write" with
      | (_ : Nothing.t) -> .)
  ;;

  (* Seekables *)

  let read_seekable_or_cancel_exn #(_await, _cancel, _t, fd, offset, bs) =
    let raw_fd = Core_unix.File_descr.to_int (Io_descr.unsafe_get fd) in
    let%with.tilde.stack bs =
      (Io_slice.Expert.with_ext_in [@kind word]) bs ~len:(fun x -> x)
    in
    let #{ mem; len } : Buf.t = (Buf.magic_of_slice [@kind bits8] [@mode local]) bs in
    let rec loop () =
      let n = Raw.pread raw_fd mem len offset in
      if n >= 0
      then Isize.of_int n
      else if n = -Syscall.eintr
      then loop ()
      else (
          match Syscall.raise_unix_error (-n) ~name:"pread" with
          | (_ : Nothing.t) -> .)
    in
    loop () [@nontail]
  ;;

  let write_seekable_or_cancel_exn #(_await, _cancel, _t, fd, offset, bs) =
    let raw_fd = Core_unix.File_descr.to_int (Io_descr.unsafe_get fd) in
    let%with.tilde.stack bs =
      (Io_slice.Expert.with_ext_out [@kind word]) (bs : Io_slice.read_only).modal
    in
    let #{ mem; len } : Buf.t =
      (Buf.magic_of_slice [@kind bits8] [@mode local read]) bs
    in
    let rec loop () =
      let n = Raw.pwrite raw_fd mem len offset in
      if n >= 0
      then Isize.of_int n
      else if n = -Syscall.eintr
      then loop ()
      else (
          match Syscall.raise_unix_error (-n) ~name:"pwrite" with
          | (_ : Nothing.t) -> .)
    in
    loop () [@nontail]
  ;;

  let resize #(_await, _t, fd, (offset : I64.t)) =
    let raw_fd = Io_descr.unsafe_get fd in
    Caml_unix.LargeFile.ftruncate raw_fd (I64.box offset);
    #()
  ;;

  let flush #(_await, _t, fd) =
    Core_unix.fsync (Io_descr.unsafe_get fd);
    #()
  ;;

  let%template stat #(_await, _t, fd) =
    let stats = Core_unix.fstat (Io_descr.unsafe_get fd) in
    File_stats.of_unix_stats stats
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  (* File system *)

  let cwd =
    (* Aliased [Io_descr]s cannot be closed accidentally. *)
    Io_descr.(create Roles.readable) ~shutdown:false (Posixat.at_fdcwd ())
  ;;

  let current_dir #(_await, _t) = cwd

  let openat ~relative_to ~follow_link path flags perm =
    let flags = Posixat.Open_flag.O_CLOEXEC :: flags in
    let flags = if follow_link then flags else O_NOFOLLOW :: flags in
    Posixat.openat
      ~dir:(Io_descr.unsafe_get relative_to)
      ~path:(path : File_path.t :> string)
      ~flags
      ~perm:(File_permissions.to_int perm)
  ;;

  let open_dir_at #(_await, _t, ~relative_to, ~follow_link, path) =
    let raw_fd =
      openat ~relative_to ~follow_link path [ O_RDONLY; O_DIRECTORY ] File_permissions.empty
    in
    try Io_descr.(create Roles.readable) ~shutdown:false raw_fd with
    | exn -> close_raw_fd_and_reraise exn raw_fd
  ;;

  let%template read_dir_at #(_await, _t, ~relative_to, ~follow_link, path) =
    let raw_fd =
      openat ~relative_to ~follow_link path [ O_RDONLY; O_DIRECTORY ] File_permissions.empty
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
    | exception exn -> close_raw_fd_and_reraise exn raw_fd
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let fstatat ~relative_to ~follow_link path =
    Posixat.fstatat
      ~dir:relative_to
      ~path
      ~flags:(if follow_link then [] else [ AT_SYMLINK_NOFOLLOW ])
  ;;

  let%template stat_at #(_await, _t, ~relative_to, ~follow_link, path) =
    let stats =
      fstatat ~relative_to:(Io_descr.unsafe_get relative_to) ~follow_link (path : File_path.t :> string)
    in
    File_stats.of_unix_stats stats
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let create_dir_at #(_await, _t, ~relative_to, path, ~parents, perm) =
    (* This logic is intended to match that of [Core_unix.mkdir] and [Core_unix.mkdir_p]
       and the [io_uring] implementation. *)
    let dir = Io_descr.unsafe_get relative_to in
    let mkdirat (path : File_path.t @ local) perm =
      Posixat.mkdirat ~dir ~path:(path : File_path.t :> string) ~perm:(File_permissions.to_int perm)
    in
    if parents
    then (
      let is_dir ~follow_link (path : File_path.t @ local) =
        match fstatat ~relative_to:dir ~follow_link (path : File_path.t :> string) with
        | { st_kind = S_DIR; _ } -> `Dir
        | _ -> `Not_dir
        | exception Caml_unix.Unix_error (ENOENT, _, _) -> `Noent
      in
      let rec mkdir_idempotent (path : File_path.t @ local) perm =
        match mkdirat path perm with
        | () -> ()
        | exception (Caml_unix.Unix_error (EISDIR, _, _)) ->
          (* [mkdir] on MacOSX returns [EISDIR] if [dirname] exists and is a directory. *)
          ()
        | exception (Caml_unix.Unix_error (error, _, _) as exn) ->
          (* When mkdir fails, we need to check if the directory already exists. We
             cannot rely on [EEXIST], because [EEXIST] only implies that the name exists,
             not that it's a directory. *)
          (match is_dir ~follow_link:true path with
           | `Dir -> ()
           | `Not_dir -> raise exn
           | `Noent ->
             if phys_equal error Caml_unix.EEXIST
             then (
               (* We raced with another process that removed the directory or
                  encountered a broken symlink. *)
               match is_dir ~follow_link:false path with
               | `Noent -> mkdir_idempotent path perm
               | `Dir | `Not_dir -> raise exn)
             else raise exn)
      in
      let rec mkdir_p (path : File_path.t @ local) perm =
        match mkdir_idempotent path perm with
        | () -> ()
        | exception (Caml_unix.Unix_error (ENOENT, _, _) as exn) ->
          (match (File_path.dirname [@alloc stack]) path with
           | None -> raise exn
           | Some dirname ->
             (* For intermediate directories, add owner write (0o200) and execute
                permissions (0o100) so that we can traverse and create directories. *)
             mkdir_p dirname File_permissions.(perm lor o_w lor o_x);
             mkdir_idempotent path perm)
      in
      mkdir_p path perm)
    else mkdirat path perm;
    #()
  ;;

  let remove_dir_at #(_await, _t, ~relative_to, path) =
    Posixat.unlinkat
      ~dir:(Io_descr.unsafe_get relative_to)
      ~path:(path : File_path.t :> string)
      ~flags:[ AT_REMOVEDIR ];
    #()
  ;;

  let remove_file_at #(_await, _t, ~relative_to, path) =
    Posixat.unlinkat ~dir:(Io_descr.unsafe_get relative_to) ~path:(path : File_path.t :> string) ~flags:[];
    #()
  ;;

  let rename_at #(_await, _t, ~relative_to_before, ~before, ~relative_to_after, ~after) =
    Posixat.renameat
      ~olddir:(Io_descr.unsafe_get relative_to_before)
      ~oldpath:(before : File_path.t :> string)
      ~newdir:(Io_descr.unsafe_get relative_to_after)
      ~newpath:(after : File_path.t :> string);
    #()
  ;;

  let create_link_at #(_await, _t, ~into, ~relative_to, path) =
    Posixat.symlinkat
      ~oldpath:into
      ~newdir:(Io_descr.unsafe_get relative_to)
      ~newpath:(path : File_path.t :> string);
    #()
  ;;

  let%template read_link_at #(_await, _t, ~relative_to, path) =
    Posixat.readlinkat ~dir:(Io_descr.unsafe_get relative_to) ~path:(path : File_path.t :> string)
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let check_is_reg raw_fd =
    let stat = Caml_unix.fstat raw_fd in
    if not (phys_equal stat.st_kind S_REG)
    then raise (Caml_unix.Unix_error (EINVAL, "openat", "not a regular file"))
  ;;

  let open_file_to_read_at #(_await, _t, ~relative_to, ~follow_link, path) =
    let raw_fd = openat ~relative_to ~follow_link path [ O_RDONLY ] File_permissions.empty in
    try
      check_is_reg raw_fd;
      Io_descr.(create Roles.(readable + writable)) ~shutdown:false raw_fd
    with
    | exn -> close_raw_fd_and_reraise exn raw_fd
  ;;

  let create_flags (create : File_create.t) : Posixat.Open_flag.t list =
    match create with
    | Never -> []
    | If_missing _ -> [ O_CREAT ]
    | Or_truncate _ -> [ O_CREAT; O_TRUNC ]
    | Always _ -> [ O_CREAT; O_EXCL ]
  ;;

  let open_or_create_file_at #(_await, _t, ~create, ~relative_to, ~follow_link, path) =
    let perm = File_create.file_permissions create in
    let raw_fd =
      openat ~relative_to ~follow_link path (O_RDWR :: create_flags create) perm
    in
    try
      check_is_reg raw_fd;
      Io_descr.(create Roles.(readable + writable)) ~shutdown:false raw_fd
    with
    | exn -> close_raw_fd_and_reraise exn raw_fd
  ;;

  let open_or_create_file_to_append_at
    #(_await, _t, ~create, ~relative_to, ~follow_link, path)
    =
    let perm = File_create.file_permissions create in
    let raw_fd =
      openat
        ~relative_to
        ~follow_link
        path
        (O_WRONLY :: O_APPEND :: create_flags create)
        perm
    in
    try
      check_is_reg raw_fd;
      Io_descr.(create Roles.writable) ~shutdown:false raw_fd
    with
    | exn -> close_raw_fd_and_reraise exn raw_fd
  ;;
end

let create = Poller.create
let close = Poller.close

let with_io await t ~f =
  let io = Io.Expert.create await (module Io_prim) t in
  f io [@nontail]
;;
