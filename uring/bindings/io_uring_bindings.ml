open Core
open Filesystem_types
open Ox
open Linux_ext
module Unix = Core_unix
module Scoped = Capsule.Scoped
module Password = Capsule.Prim.Password
module Key = Capsule.Prim.Key
module Data = Capsule.Data
open C_intf

type ring : value mod aliased portable unyielding

module Mem = struct
  include Mem

  let malloc : i64 -> t @ unique =
    fun size ->
    let open struct
      external malloc
        :  i64
        -> Mem.t @ unique
        @@ portable
        = "caml_no_bytecode_impl" "malloc"
      [@@noalloc]
    end in
    let t = malloc size in
    if Mem.is_null (borrow_ t)
    then (
      match raise Out_of_memory with
      | (_ : Nothing.t) -> .)
    else t
  ;;

  external free : Mem.t @ unique -> unit# @@ portable = "caml_no_bytecode_impl" "free"
  [@@noalloc]

  let free_aliased t = free (Obj.magic_unique t)

  let%template[@inline] with_malloc
    : i64 -> f:(t @ local -> 'a @ l) @ local once -> 'a @ l
    =
    fun size ~f ->
    let t = malloc size in
    match[@exclave_if_stack a] f t with
    | result ->
      let #() = free_aliased t in
      result
    | exception exn ->
      let bt = Backtrace.Exn.most_recent () in
      let #() = free_aliased t in
      Exn.raise_with_original_backtrace exn bt
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;
end

module Result = struct
  include Core_unix.Syscall_result.Int

  let ecanceled = Private.of_int (-I64.to_int_exn c_ECANCELED)
  let is_already (result : t) = (result :> int) = -I64.to_int_exn c_EALREADY
  let is_canceled (result : t) = (result :> int) = -I64.to_int_exn c_ECANCELED
  let is_exist (result : t) = (result :> int) = -I64.to_int_exn c_EEXIST
  let is_isdir (result : t) = (result :> int) = -I64.to_int_exn c_EISDIR
  let is_noent (result : t) = (result :> int) = -I64.to_int_exn c_ENOENT

  let[@inline] is_more_specific result ~than:before =
    (not (equal before result))
    && ((is_ok before && (equal before (create_ok 0) || is_error result))
        || (is_canceled before && is_error result))
  ;;
end

module Setup_flags = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)
  let sqpoll = of_int64_u c_IORING_SETUP_SQPOLL
  let sq_aff = of_int64_u c_IORING_SETUP_SQ_AFF
  let cqsize = of_int64_u c_IORING_SETUP_CQSIZE
  let clamp_ = of_int64_u c_IORING_SETUP_CLAMP
  let attach_wq = of_int64_u c_IORING_SETUP_ATTACH_WQ
  let submit_all = of_int64_u c_IORING_SETUP_SUBMIT_ALL
  let coop_taskrun = of_int64_u c_IORING_SETUP_COOP_TASKRUN
  let no_sqarray = of_int64_u c_IORING_SETUP_NO_SQARRAY

  include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = true

      let known =
        [ sqpoll, "sqpoll"
        ; sq_aff, "sq_aff"
        ; cqsize, "cqsize"
        ; clamp_, "clamp"
        ; attach_wq, "attach_wq"
        ; submit_all, "submit_all"
        ; coop_taskrun, "coop_taskrun"
        ; no_sqarray, "no_sqarray"
        ]
      ;;
    end)
end

module Params : sig @@ portable
  type t : bits64 mod external_ many stateless unyielding

  val empty : unit# -> t @ local
  val add_flags : t @ local -> Setup_flags.t -> unit
  val set_cq_entries : t @ local -> int -> unit
  val set_sq_thread_idle : t @ local -> int -> unit
  val set_sq_thread_cpu : t @ local -> int -> unit
  val set_wq_fd : t @ local -> Unix.File_descr.t -> unit
end = struct
  type t = Mem.t

  let empty #() = exclave_
    let len = c_io_uring_params in
    let t = Mem.alloc_stack ~len in
    Mem.fill t ~len U8.zero;
    t
  ;;

  let set_flags t v =
    Mem.set_i32
      (Mem.add t c_io_uring_params__flags)
      (I32.of_int_trunc (Setup_flags.to_int_exn v)) [@nontail]
  ;;

  let get_flags t =
    Mem.get_i32 (Mem.add t c_io_uring_params__flags)
    |> I32.to_int_trunc
    |> Setup_flags.of_int
  ;;

  let add_flags t v = set_flags t Setup_flags.(get_flags t + v)

  let set_cq_entries t v =
    Mem.set_i32 (Mem.add t c_io_uring_params__cq_entries) (I32.of_int_exn v);
    add_flags t Setup_flags.cqsize [@nontail]
  ;;

  let set_sq_thread_idle t v =
    Mem.set_i32 (Mem.add t c_io_uring_params__sq_thread_idle) (I32.of_int_exn v);
    add_flags t Setup_flags.sqpoll [@nontail]
  ;;

  let set_sq_thread_cpu t v =
    assert%debug (Setup_flags.(do_intersect sqpoll) (get_flags t));
    Mem.set_i32 (Mem.add t c_io_uring_params__sq_thread_cpu) (I32.of_int_exn v);
    add_flags t Setup_flags.sq_aff [@nontail]
  ;;

  let set_wq_fd t (v : Unix.File_descr.t) =
    Mem.set_i32 (Mem.add t c_io_uring_params__wq_fd) (Unix.File_descr.to_i32 v);
    add_flags t Setup_flags.attach_wq [@nontail]
  ;;
end

module Io_uring = struct
  type t = Mem.t

  let malloc #() = Mem.malloc c_io_uring
  let free_aliased = Mem.free_aliased
  let ring_fd t = Unix.File_descr.of_i32 (Mem.get_i32 (Mem.add t c_io_uring__ring_fd))

  let sq_entries t =
    (* [sq.ring_entries] is read only after the ring has been initialized. *)
    let t = Obj.magic_uncontended t in
    Mem.get_i32 (Mem.add (Mem.add t c_io_uring__sq) c_io_uring_sq__ring_entries)
  ;;

  let to_ring t : ring = Obj.magic I64.(to_int_trunc (Mem.to_raw_address t lsr 1))

  let%template of_ring (ring : ring @ c) =
    Mem.create I64.(of_int ((Obj.magic [@mode c]) ring : int) lsl 1)
  [@@mode c = (uncontended, contended)]
  ;;

  let init ~attach ~sq_entries ~cq_entries ~sqpoll io_uring =
    let sq_entries =
      match sq_entries with
      | Null -> Isize.of_int 128
      | This sq_entries -> Isize.of_int sq_entries
    in
    let params =
      let t = Params.empty #() in
      (* Note that [Io_uring_shards] assumes [submit_all] has been specified. *)
      Params.add_flags t Setup_flags.(clamp_ + coop_taskrun + submit_all);
      (Or_null.iter [@mode local]) cq_entries ~f:[%eta1 Params.set_cq_entries t];
      (Option.iter [@mode local]) sqpoll ~f:(fun (~idle_ms, ~cpu_aff) ->
        Params.set_sq_thread_idle t idle_ms;
        (Or_null.iter [@mode local])
          cpu_aff
          ~f:[%eta1 Params.set_sq_thread_cpu t] [@nontail]);
      (Option.iter [@mode local]) attach ~f:[%eta1 Params.set_wq_fd t];
      t
    in
    let result =
      let open struct
        external io_uring_queue_init_params
          :  sq_entries:isize
          -> t
          -> Params.t @ local
          -> i32
          @@ portable
          = "caml_no_bytecode_impl" "io_uring_queue_init_params"
        [@@noalloc]
      end in
      io_uring_queue_init_params ~sq_entries io_uring params
      |> I32.to_int_trunc
      |> Result.Private.of_int
    in
    if Result.is_error result
    then
      raise (Unix.Unix_error (Result.error_exn result, "io_uring_queue_init_params", ""))
  ;;

  external exit : t -> unit# @@ portable = "caml_no_bytecode_impl" "io_uring_queue_exit"
  [@@noalloc]
end

module Tag = struct
  type t = nativeint_u
end

let () =
  assert%debug (I64.O.(c_sa_family_t = #2L));
  assert%debug (I64.O.(c_socklen_t = #4L));
  ()
;;

module Socklen_ref = struct
  type t = Mem.t

  let empty #() = exclave_ Mem.alloc_stack ~len:c_socklen_t
  let get t = I64.of_i32 (Mem.get_i32 t)

  let set t len =
    Mem.set_i32 t (I64.to_i32_exn len);
    #()
  ;;

  let unsafe_globalize = Mem.unsafe_globalize
end

module Sockaddr_ref = struct
  type t = Buf.t

  let for_sockaddr #() = exclave_ Buf.alloc_stack ~len:c_sockaddr_in
  let set_socklen t socklen = Socklen_ref.set socklen (Buf.length t)

  let set_to_sockaddr (#{ mem; len } : t) sockaddr =
    let open struct
      external set_to_sockaddr_unsafe
        :  Mem.t @ local
        -> Sockaddr.t
        -> unit#
        @@ stateless
        = "caml_no_bytecode_impl" "ocaml_io_uring_sockaddr_in_set_noalloc"
      [@@noalloc]
    end in
    if I64.O.(I64.of_isize len < c_sockaddr_in)
    then invalid_arg "set_to_sockaddr: invalid socket address length";
    set_to_sockaddr_unsafe mem sockaddr
  ;;

  let get_as_sockaddr (#{ mem; len } : t) socklen =
    let open struct
      external get_as_sockaddr_unsafe
        :  Mem.t @ local read
        -> Sockaddr.t @ unique
        @@ stateless
        = "caml_no_bytecode_impl" "ocaml_io_uring_sockaddr_in_get_noalloc"
      [@@noalloc]
    end in
    if I64.O.(I64.of_isize len < Socklen_ref.get socklen)
       || I64.O.(Socklen_ref.get socklen <> c_sockaddr_in)
    then invalid_arg "get_as_sockaddr: invalid socket address length";
    if not
         (I16.equal
            (Mem.get_i16 ((Mem.add [@mode read]) mem c_sockaddr_in__sin_family))
            (I16.of_i64_trunc c_AF_INET))
    then invalid_arg "get_as_sockaddr: invalid socket address family";
    get_as_sockaddr_unsafe mem
  ;;

  let unsafe_globalize = Buf.unsafe_globalize
end

module Iovec = struct
  type t = Buf.t

  let%template make
    (bufs : (Buf.t Modes.t[@modality global] [@kind bits64 & word]) Iarray.t @ local v)
    = exclave_
    let n = Iarray.length bufs in
    let t = Buf.alloc_stack ~len:I64.O.(c_iovec * I64.of_int n) in
    for i = 0 to n - 1 do
      let open struct
        external magic_read_write : 'a @ local read -> 'a @ local = "%identity"
      end in
      let buf = Iarray.get (magic_read_write bufs) i in
      Buf.Unchecked.set_i64
        t
        I64.O.((I64.of_int i * c_iovec) + c_iovec__iov_base)
        (Mem.to_raw_address (Buf.mem buf.modal));
      Buf.Unchecked.set_i64
        t
        I64.O.((I64.of_int i * c_iovec) + c_iovec__iov_len)
        (Buf.length buf.modal)
    done;
    t
  [@@mode v = (read_write, read)]
  ;;

  let%template unsafe_globalize = (Buf.unsafe_globalize [@mode v])
  [@@mode v = (read_write, read)]
  ;;
end

module Msghdr = struct
  type t = Mem.t

  let socklen t = exclave_ Mem.add t c_msghdr__msg_namelen

  let%template make (sockaddr_ref @ v) (iovec @ v) = exclave_
    let t = Mem.alloc_stack ~len:c_msghdr in
    Mem.set_i64
      (Mem.add t c_msghdr__msg_name)
      (Mem.to_raw_address ((Buf.mem [@mode v]) sockaddr_ref));
    let #() = Socklen_ref.set (socklen t) (Buf.length sockaddr_ref) in
    Mem.set_i64
      (Mem.add t c_msghdr__msg_iov)
      (Mem.to_raw_address ((Buf.mem [@mode v]) iovec));
    Mem.set_i64 (Mem.add t c_msghdr__msg_iovlen) I64.O.(Buf.length iovec / c_iovec);
    Mem.set_i64 (Mem.add t c_msghdr__msg_control) I64.zero;
    Mem.set_i64 (Mem.add t c_msghdr__msg_controllen) I64.zero;
    Mem.set_i32 (Mem.add t c_msghdr__msg_flags) I32.zero;
    t
  [@@mode v = (read_write, read)]
  ;;

  let%template unsafe_globalize = (Mem.unsafe_globalize [@mode v])
  [@@mode v = (read_write, read)]
  ;;
end

module Statx_ref = struct
  type t = Mem.t

  let empty #() = exclave_ Mem.alloc_stack ~len:c_statx

  let%template[@inline] with_malloc ~f =
    (Mem.with_malloc [@alloc a]) c_statx ~f [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let get_kind t : File_kind.t =
    let mode = Mem.get_u16 (Mem.add t c_statx__stx_mode) |> U16.to_int in
    let kind = mode land I64.to_int_exn c_S_IFMT in
    if kind = I64.to_int_exn c_S_IFBLK
    then Block_device
    else if kind = I64.to_int_exn c_S_IFCHR
    then Character_device
    else if kind = I64.to_int_exn c_S_IFDIR
    then Directory
    else if kind = I64.to_int_exn c_S_IFIFO
    then Fifo
    else if kind = I64.to_int_exn c_S_IFREG
    then Regular
    else if kind = I64.to_int_exn c_S_IFSOCK
    then Socket
    else if kind = I64.to_int_exn c_S_IFLNK
    then Symlink
    else failwith "Uring.Statx_ref.get: unknown file kind"
  ;;

  let get_permissions t =
    let mode = Mem.get_u16 (Mem.add t c_statx__stx_mode) |> U16.to_int in
    File_permissions.of_int_exn (mode land lnot (I64.to_int_exn c_S_IFMT))
  ;;

  let%template get t =
    let open struct
      external get_device
        :  Mem.t @ local read
        -> major_offset:I64.t
        -> minor_offset:I64.t
        -> int
        @@ stateless
        = "caml_no_bytecode_impl" "ocaml_io_uring_get_device_noalloc"
      [@@noalloc]
    end in
    let time tv =
      let sec = I64.to_int_exn (Mem.get_i64 (Mem.add tv c_statx_timestamp__tv_sec)) in
      let ns = U32.to_int_exn (Mem.get_u32 (Mem.add tv c_statx_timestamp__tv_nsec)) in
      Time_ns.of_span_since_epoch (Time_ns.Span.create ~sec ~ns ())
    in
    let host_device =
      get_device
        t
        ~major_offset:c_statx__stx_dev_major
        ~minor_offset:c_statx__stx_dev_minor
    in
    let inode = I64.to_int_exn (Mem.get_i64 (Mem.add t c_statx__stx_ino)) in
    let permissions = get_permissions t in
    let kind = get_kind t in
    let hard_links = U32.to_int_exn (Mem.get_u32 (Mem.add t c_statx__stx_nlink)) in
    let user_id = U32.to_int_exn (Mem.get_u32 (Mem.add t c_statx__stx_uid)) in
    let group_id = U32.to_int_exn (Mem.get_u32 (Mem.add t c_statx__stx_gid)) in
    let file_device =
      get_device
        t
        ~major_offset:c_statx__stx_rdev_major
        ~minor_offset:c_statx__stx_rdev_minor
    in
    let size = Mem.get_i64 (Mem.add t c_statx__stx_size) in
    let access_time = time (Mem.add t c_statx__stx_atime) in
    let modify_time = time (Mem.add t c_statx__stx_mtime) in
    let status_time = time (Mem.add t c_statx__stx_ctime) in
    File_stats.
      { host_device
      ; inode
      ; kind
      ; permissions
      ; hard_links
      ; user_id
      ; group_id
      ; file_device
      ; size
      ; access_time
      ; modify_time
      ; status_time
      } [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let unsafe_globalize = Mem.unsafe_globalize
end

module String_ref = struct
  type t = Buf.t

  let of_string string = exclave_
    let open struct
      external is_c_safe
        :  string @ local
        -> I32.t
        = "caml_no_bytecode_impl" "caml_string_is_c_safe"
      [@@noalloc]
    end in
    if I32.(zero = is_c_safe string) then invalid_arg "String_ref: string is not C-safe";
    let len = String.length string in
    let t = Buf.alloc_stack ~len:(I64.of_int (len + 1)) in
    (* We rely on the [string] having an extra null byte at the end. *)
    Mem.copy_from_string
      ~src:string
      ~dst:((Buf.mem [@mode local]) t)
      ~len:(I64.of_int (len + 1));
    t
  ;;

  let of_file_path file_path = exclave_ of_string (file_path : File_path.t :> string)
  let unsafe_globalize = Buf.unsafe_globalize
end

module Open_mode = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)

  type t = Int63.t

  let rdonly = of_int64_u c_O_RDONLY
  let wronly = of_int64_u c_O_WRONLY
  let rdwr = of_int64_u c_O_RDWR
end

module Open_flags = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)

  (* Creation flags *)

  let cloexec = of_int64_u c_O_CLOEXEC
  let creat = of_int64_u c_O_CREAT
  let directory = of_int64_u c_O_DIRECTORY
  let excl = of_int64_u c_O_EXCL
  let noctty = of_int64_u c_O_NOCTTY
  let nofollow = of_int64_u c_O_NOFOLLOW
  let tmpfile = of_int64_u c_O_TMPFILE
  let trunc = of_int64_u c_O_TRUNC

  (* Status flags *)

  let append = of_int64_u c_O_APPEND
  let async = of_int64_u c_O_ASYNC
  let direct = of_int64_u c_O_DIRECT
  let dsync = of_int64_u c_O_DSYNC
  let largefile = of_int64_u c_O_LARGEFILE
  let noatime = of_int64_u c_O_NOATIME
  let nonblock = of_int64_u c_O_NONBLOCK
  let path = of_int64_u c_O_PATH
  let sync = of_int64_u c_O_SYNC

  include Flags.Make (struct
      let allow_intersecting = true
      let should_print_error = true
      let remove_zero_flags = true

      let known =
        [ (* Creation flags *)
          cloexec, "cloexec"
        ; creat, "creat"
        ; directory, "directory"
        ; excl, "excl"
        ; noctty, "noctty"
        ; nofollow, "nofollow"
        ; tmpfile, "tmpfile"
        ; trunc, "trunc"
        ; (* Status flags *)
          append, "append"
        ; async, "async"
        ; direct, "direct"
        ; dsync, "dsync"
        ; largefile, "largefile"
        ; noatime, "noatime"
        ; nonblock, "nonblock"
        ; path, "path"
        ; sync, "sync"
        ]
      ;;
    end)
end

module Socket_domain = struct
  type t = int

  let unix = I64.to_int_exn c_AF_UNIX
  let inet = I64.to_int_exn c_AF_INET
  let inet6 = I64.to_int_exn c_AF_INET6
end

module Socket_type = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)

  type t = Int63.t

  let stream = of_int64_u c_SOCK_STREAM
  let dgram = of_int64_u c_SOCK_DGRAM
  let raw = of_int64_u c_SOCK_RAW
  let seqpacket = of_int64_u c_SOCK_SEQPACKET
end

module Socket_flags = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)
  let nonblock = of_int64_u c_SOCK_NONBLOCK
  let cloexec = of_int64_u c_SOCK_CLOEXEC

  include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = false
      let known = [ nonblock, "nonblock"; cloexec, "cloexec" ]
    end)
end

module Recv_flags = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)
  let dontwait = of_int64_u c_MSG_DONTWAIT
  let oob = of_int64_u c_MSG_OOB
  let peek = of_int64_u c_MSG_PEEK
  let trunc = of_int64_u c_MSG_TRUNC
  let waitall = of_int64_u c_MSG_WAITALL

  include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = false

      let known =
        [ dontwait, "dontwait"
        ; oob, "oob"
        ; peek, "peek"
        ; trunc, "trunc"
        ; waitall, "waitall"
        ]
      ;;
    end)
end

module Send_flags = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)
  let dontwait = of_int64_u c_MSG_DONTWAIT
  let oob = of_int64_u c_MSG_OOB
  let confirm = of_int64_u c_MSG_CONFIRM
  let dontroute = of_int64_u c_MSG_DONTROUTE
  let eor = of_int64_u c_MSG_EOR
  let more = of_int64_u c_MSG_MORE
  let nosignal = of_int64_u c_MSG_NOSIGNAL
  let fastopen = of_int64_u c_MSG_FASTOPEN

  include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = false

      let known =
        [ dontwait, "dontwait"
        ; oob, "oob"
        ; confirm, "confirm"
        ; dontroute, "dontroute"
        ; eor, "eor"
        ; more, "more"
        ; nosignal, "nosignal"
        ; fastopen, "fastopen"
        ]
      ;;
    end)
end

module Statx_flags = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)
  let basic_stats = of_int64_u c_STATX_BASIC_STATS
  let type_ = of_int64_u c_STATX_TYPE

  include Flags.Make (struct
      let allow_intersecting = true
      let should_print_error = true
      let remove_zero_flags = true
      let known = [ basic_stats, "basic_stats"; type_, "type" ]
    end)
end

module At_flags = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)
  let symlink_nofollow = of_int64_u c_AT_SYMLINK_NOFOLLOW
  let removedir = of_int64_u c_AT_REMOVEDIR

  include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = true
      let known = [ symlink_nofollow, "symlink_nofollow"; removedir, "removedir" ]
    end)
end

module Sqe_flags = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)
  let async = of_int64_u c_IOSQE_ASYNC
  let cqe_skip_success = of_int64_u c_IOSQE_CQE_SKIP_SUCCESS
  let io_drain = of_int64_u c_IOSQE_IO_DRAIN
  let io_hardlink = of_int64_u c_IOSQE_IO_HARDLINK
  let io_link = of_int64_u c_IOSQE_IO_LINK

  include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = true

      let known =
        [ async, "async"
        ; cqe_skip_success, "cqe_skip_success"
        ; io_drain, "io_drain"
        ; io_hardlink, "io_hardlink"
        ; io_link, "io_link"
        ]
      ;;
    end)
end

module Cancel_flags = struct
  let of_int64_u x = Int63.of_int (I64.to_int_exn x)
  let all = of_int64_u c_IORING_ASYNC_CANCEL_ALL
  let any = of_int64_u c_IORING_ASYNC_CANCEL_ANY
  let userdata = of_int64_u c_IORING_ASYNC_CANCEL_USERDATA

  include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = true
      let known = [ all, "all"; any, "any"; userdata, "userdata" ]
    end)
end

module Sq = struct
  type t = T of ring [@@unboxed]

  let entries sq =
    let (T ring) = Capsule.Prim.Data.Local.project sq in
    I32.to_int_trunc (Io_uring.sq_entries ((Io_uring.of_ring [@mode contended]) ring))
  ;;

  let submit (Scoped.P #{ data; password }) =
    let open struct
      external submit
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> Result.t
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_submit_noalloc"
      [@@noalloc]
    end in
    let result = submit ~password data in
    if Result.is_error result
    then raise (Core_unix.Unix_error (Result.error_exn result, "submit", ""))
  ;;

  let ensure_can_push (Scoped.P #{ data; password }) ~n =
    assert%debug (n > 0);
    let open struct
      external ensure_can_push
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> n:int
        -> Result.t
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_ensure_can_push_noalloc"
      [@@noalloc]
    end in
    let result = ensure_can_push ~password data ~n in
    if Result.is_error result
    then raise (Core_unix.Unix_error (Result.error_exn result, "ensure_can_push", ""))
  ;;

  let push_cancel
    (Scoped.P #{ data; password })
    ~tag
    sqe_flags
    ~tag_to_cancel
    cancel_flags
    =
    let open struct
      external push_cancel
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> tag_to_cancel:Tag.t
        -> cancel_flags:Cancel_flags.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_cancel_noalloc"
      [@@noalloc]
    end in
    let #() = push_cancel ~password data ~tag ~sqe_flags ~tag_to_cancel ~cancel_flags in
    ()
  ;;

  let push_nop (Scoped.P #{ data; password }) ~tag sqe_flags =
    let open struct
      external push_nop
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_nop_noalloc"
      [@@noalloc]
    end in
    let #() = push_nop ~password data ~tag ~sqe_flags in
    ()
  ;;

  let push_close (Scoped.P #{ data; password }) ~tag sqe_flags fd =
    let open struct
      external push_close
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_close_noalloc"
      [@@noalloc]
    end in
    let #() = push_close ~password data ~tag ~sqe_flags fd in
    ()
  ;;

  let push_shutdown (Scoped.P #{ data; password }) ~tag sqe_flags fd flags =
    let open struct
      external push_shutdown
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> Core_unix.shutdown_command
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_shutdown_noalloc"
      [@@noalloc]
    end in
    let #() = push_shutdown ~password data ~tag ~sqe_flags fd flags in
    ()
  ;;

  let push_socket
    (Scoped.P #{ data; password })
    ~tag
    sqe_flags
    socket_domain
    socket_type
    socket_flags
    ~protocol
    =
    let open struct
      external push_socket
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Socket_domain.t
        -> socket_type_and_flags:Int63.t
        -> protocol:int
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_socket_noalloc"
      [@@noalloc]
    end in
    let socket_type_and_flags = Socket_flags.(socket_type + socket_flags) in
    let #() =
      push_socket
        ~password
        data
        ~tag
        ~sqe_flags
        socket_domain
        ~socket_type_and_flags
        ~protocol
    in
    ()
  ;;

  let push_connect (Scoped.P #{ data; password }) ~tag sqe_flags fd sockaddr socklen =
    let open struct
      external push_connect
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> Mem.t @ read
        -> Socklen_ref.t @ read
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_connect_noalloc"
      [@@noalloc]
    end in
    let #{ mem = sockaddr; len } : Sockaddr_ref.t = sockaddr in
    assert%debug (I64.O.(Socklen_ref.get socklen <= I64.of_isize len));
    let #() = push_connect ~password data ~tag ~sqe_flags fd sockaddr socklen in
    ()
  ;;

  let push_bind (Scoped.P #{ data; password }) ~tag sqe_flags fd sockaddr socklen =
    let open struct
      external push_bind
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> Mem.t @ read
        -> Socklen_ref.t @ read
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_bind_noalloc"
      [@@noalloc]
    end in
    let #{ mem = sockaddr; len } : Sockaddr_ref.t = sockaddr in
    assert%debug (I64.O.(Socklen_ref.get socklen <= I64.of_isize len));
    let #() = push_bind ~password data ~tag ~sqe_flags fd sockaddr socklen in
    ()
  ;;

  let push_listen (Scoped.P #{ data; password }) ~tag sqe_flags fd ~max_pending_requests =
    let open struct
      external push_listen
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> int
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_listen_noalloc"
      [@@noalloc]
    end in
    let #() = push_listen ~password data ~tag ~sqe_flags fd max_pending_requests in
    ()
  ;;

  let push_accept
    (Scoped.P #{ data; password })
    ~tag
    sqe_flags
    fd
    sockaddr
    socklen
    socket_flags
    =
    let open struct
      external push_accept
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> Mem.t
        -> Socklen_ref.t
        -> socket_flags:Socket_flags.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_accept_noalloc"
      [@@noalloc]
    end in
    let #{ mem = sockaddr; len } : Sockaddr_ref.t = sockaddr in
    assert%debug (I64.O.(Socklen_ref.get socklen = I64.of_isize len));
    let #() =
      push_accept ~password data ~tag ~sqe_flags fd sockaddr socklen ~socket_flags
    in
    ()
  ;;

  let push_recvmsg (Scoped.P #{ data; password }) ~tag sqe_flags fd msghdr recv_flags =
    let open struct
      external push_recvmsg
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> Msghdr.t
        -> recv_flags:Recv_flags.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_recvmsg_noalloc"
      [@@noalloc]
    end in
    let #() = push_recvmsg ~password data ~tag ~sqe_flags fd msghdr ~recv_flags in
    ()
  ;;

  let push_sendmsg (Scoped.P #{ data; password }) ~tag sqe_flags fd msghdr send_flags =
    let open struct
      external push_sendmsg
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> Msghdr.t @ read
        -> send_flags:Send_flags.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_sendmsg_noalloc"
      [@@noalloc]
    end in
    let #() = push_sendmsg ~password data ~tag ~sqe_flags fd msghdr ~send_flags in
    ()
  ;;

  let push_read (Scoped.P #{ data; password }) ~tag sqe_flags fd ~offset bs =
    let open struct
      external push_read
        : ('b : value_or_null).
        password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> offset:I64.t
        -> mem:Mem.t
        -> len:Isize.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_read_noalloc"
      [@@noalloc]
    end in
    let #{ mem; len } : Buf.t = bs in
    let #() = push_read ~password data ~tag ~sqe_flags fd ~offset ~mem ~len in
    ()
  ;;

  let push_write (Scoped.P #{ data; password }) ~tag sqe_flags fd ~offset bs =
    let open struct
      external push_write
        : ('b : value_or_null).
        password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> offset:I64.t
        -> mem:Mem.t @ read
        -> len:Isize.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_write_noalloc"
      [@@noalloc]
    end in
    let #{ mem; len } : Buf.t = bs in
    let #() = push_write ~password data ~tag ~sqe_flags fd ~offset ~mem ~len in
    ()
  ;;

  let push_poll_add (Scoped.P #{ data; password }) ~tag sqe_flags fd flags =
    let open struct
      external push_poll_add
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> int
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_poll_add_noalloc"
      [@@noalloc]
    end in
    let #() =
      push_poll_add ~password data ~tag ~sqe_flags fd (Epoll.Flags.to_int_exn flags)
    in
    ()
  ;;

  let push_fsync (Scoped.P #{ data; password }) ~tag sqe_flags fd =
    let open struct
      external push_fsync
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_fsync_noalloc"
      [@@noalloc]
    end in
    let #() = push_fsync ~password data ~tag ~sqe_flags fd in
    ()
  ;;

  let push_ftruncate (Scoped.P #{ data; password }) ~tag sqe_flags fd size =
    let open struct
      external push_ftruncate
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> I64.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_ftruncate_noalloc"
      [@@noalloc]
    end in
    let #() = push_ftruncate ~password data ~tag ~sqe_flags fd size in
    ()
  ;;

  let push_openat
    (Scoped.P #{ data; password })
    ~tag
    sqe_flags
    ~relative_to
    ~path
    mode
    flags
    perm
    =
    let open struct
      external push_openat
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> Mem.t @ local read
        -> mode_and_flags:Int63.t
        -> File_permissions.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_openat_noalloc"
      [@@noalloc]
    end in
    let #{ mem = path; _ } : Buf.t = path in
    let mode_and_flags = Open_flags.(mode + flags) in
    let #() =
      push_openat ~password data ~tag ~sqe_flags relative_to path ~mode_and_flags perm
    in
    ()
  ;;

  let push_unlinkat
    (Scoped.P #{ data; password })
    ~tag
    sqe_flags
    ~relative_to
    ~path
    at_flags
    =
    let open struct
      external push_unlinkat
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> Unix.File_descr.t
        -> Mem.t @ local read
        -> at_flags:Int63.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_unlinkat_noalloc"
      [@@noalloc]
    end in
    let #{ mem = path; _ } : Buf.t = path in
    let #() = push_unlinkat ~password data ~tag ~sqe_flags relative_to path ~at_flags in
    ()
  ;;

  let push_renameat
    sq
    ~tag
    sqe_flags
    ~relative_to_before
    ~before
    ~relative_to_after
    ~after
    =
    let open struct
      external push_renameat
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> relative_to_before:Unix.File_descr.t
        -> before:Mem.t @ local read
        -> relative_to_after:Unix.File_descr.t
        -> after:Mem.t @ local read
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_renameat_noalloc"
      [@@noalloc]
    end in
    let (Scoped.P #{ data; password }) = sq in
    let #{ mem = before; _ } : Buf.t = before in
    let #{ mem = after; _ } : Buf.t = after in
    let #() =
      push_renameat
        ~password
        data
        ~tag
        ~sqe_flags
        ~relative_to_before
        ~before
        ~relative_to_after
        ~after
    in
    ()
  ;;

  let push_symlinkat
    (Scoped.P #{ data; password })
    ~tag
    sqe_flags
    ~into
    ~relative_to
    ~path
    =
    let open struct
      external push_symlinkat
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> into:Mem.t @ local read
        -> relative_to:Unix.File_descr.t
        -> path:Mem.t @ local read
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_symlinkat_noalloc"
      [@@noalloc]
    end in
    let #{ mem = into; _ } : Buf.t = into in
    let #{ mem = path; _ } : Buf.t = path in
    let #() = push_symlinkat ~password data ~tag ~sqe_flags ~into ~relative_to ~path in
    ()
  ;;

  let push_mkdirat (Scoped.P #{ data; password }) ~tag sqe_flags ~relative_to ~path perm =
    let open struct
      external push_mkdirat
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> relative_to:Unix.File_descr.t
        -> path:Mem.t @ local read
        -> File_permissions.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_mkdirat_noalloc"
      [@@noalloc]
    end in
    let #{ mem = path; _ } : Buf.t = path in
    let #() = push_mkdirat ~password data ~tag ~sqe_flags ~relative_to ~path perm in
    ()
  ;;

  let push_statx
    (Scoped.P #{ data; password })
    ~tag
    sqe_flags
    ~relative_to
    ~path
    at_flags
    statx_flags
    statx
    =
    let open struct
      external push_statx
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> tag:Tag.t
        -> sqe_flags:Sqe_flags.t
        -> relative_to:Unix.File_descr.t
        -> path:Mem.t @ local read
        -> at_flags:At_flags.t
        -> statx_flags:Statx_flags.t
        -> Statx_ref.t
        -> unit#
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_push_statx_noalloc"
      [@@noalloc]
    end in
    let #{ mem = path; _ } : Buf.t = path in
    let #() =
      push_statx
        ~password
        data
        ~tag
        ~sqe_flags
        ~relative_to
        ~path
        ~at_flags
        ~statx_flags
        statx
    in
    ()
  ;;
end

module Cq = struct
  type t = T of ring [@@unboxed]

  let register_eventfd (Scoped.P #{ data; password }) eventfd =
    let open struct
      external register_eventfd
        :  password:'k Password.t @ local
        -> (t, 'k) Data.t @ local
        -> Eventfd.t
        -> Result.t
        @@ portable
        = "caml_no_bytecode_impl" "ocaml_io_uring_register_eventfd_noalloc"
      [@@noalloc]
    end in
    register_eventfd ~password data eventfd
  ;;

  module Entry = struct
    type t =
      #{ user_data : Tag.t
       ; res_and_flags : int64_u
       }

    let[@inline] tag t = t.#user_data

    let[@inline] result t =
      t.#res_and_flags |> I64.to_i32_trunc |> I32.to_int_trunc |> Result.Private.of_int
    ;;

    module Flags = struct
      let of_int64_u x = Int63.of_int (I64.to_int_exn x)
      let buffer = of_int64_u c_IORING_CQE_F_BUFFER
      let more = of_int64_u c_IORING_CQE_F_MORE
      let sock_nonempty = of_int64_u c_IORING_CQE_F_SOCK_NONEMPTY
      let notif = of_int64_u c_IORING_CQE_F_NOTIF
      let buf_more = of_int64_u c_IORING_CQE_F_BUF_MORE

      include Flags.Make (struct
          let allow_intersecting = false
          let should_print_error = true
          let remove_zero_flags = true

          let known =
            [ buffer, "buffer"
            ; more, "more"
            ; sock_nonempty, "sock_nonempty"
            ; notif, "notif"
            ; buf_more, "buf_more"
            ]
          ;;
        end)
    end

    let[@inline] flags t =
      Int63.of_int (I64.to_int_trunc I64.O.((t.#res_and_flags lsl 16) lsr 48))
    ;;

    let[@inline] buffer t =
      if Flags.do_intersect (flags t) Flags.buffer
      then This (I64.to_int_trunc I64.O.(t.#res_and_flags lsr 48))
      else Null
    ;;
  end

  type ('a : word & bits64) list =
    | []
    | ( :: ) of 'a * 'a list

  external drain
    :  t @ local
    -> Entry.t list @ local unique
    @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_uring_drain_noalloc"
  [@@noalloc]
end

let create ~attach ~sq_entries ~cq_entries ~sqpoll () =
  let io_uring = Io_uring.malloc #() in
  let attach =
    (Option.map [@mode local] [@kind (value_or_null & void) value_or_null])
      attach
      ~f:(fun sq ->
        Capsule.Scoped.get sq ~f:(fun (Sq.T ring) -> Io_uring.(ring_fd (of_ring ring)))
        [@nontail])
  in
  match Io_uring.init ~attach ~sq_entries ~cq_entries ~sqpoll io_uring with
  | () ->
    let ring = Io_uring.to_ring io_uring in
    let sq = Capsule.Owned.create (fun () -> Sq.T (Obj.magic_uncontended ring)) in
    let cq = Capsule.Owned.create (fun () -> Cq.T (Obj.magic_uncontended ring)) in
    #(~sq, ~cq)
  | exception exn ->
    let bt = Backtrace.Exn.most_recent () in
    let #() = Io_uring.free_aliased io_uring in
    (match Exn.raise_with_original_backtrace exn bt with
     | (_ : Nothing.t) -> .)
;;

let exit #(~sq, ~cq) =
  let (P #{ data; key }) = Capsule.Owned.to_repr cq in
  let access = Key.destroy key in
  let (Cq.T ring) = Data.unwrap ~access data in
  assert%debug (
    let (P #{ data; key }) = Capsule.Owned.to_repr sq in
    let access = Key.destroy key in
    let (Sq.T ring') = Data.unwrap ~access data in
    phys_equal ring ring');
  let io_uring = Io_uring.of_ring ring in
  let #() = Io_uring.exit io_uring in
  let #() = Io_uring.free_aliased io_uring in
  ()
;;
