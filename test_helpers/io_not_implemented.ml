open Base
open Io_kernel

let[@inline never] not_implemented #() = failwith "Not implemented"

module Io_prim : Io.Prim.S with type t = unit = struct
  type t = unit

  let close _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let stream_addrs _ = not_implemented #()
  let connect_or_cancel_exn _ = not_implemented #()
  let listen_or_cancel_exn _ = not_implemented #()

  let accept_or_cancel_exn _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let datagram_addrs _ = not_implemented #()
  let bind_datagram_socket_or_cancel_exn _ = not_implemented #()

  let recv_from_or_cancel_exn _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let is_nonblocking _ = not_implemented #()

  let recv_from_nonblocking _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let send_to_or_cancel_exn _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let send_to_nonblocking _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let addr_info _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let addr _ = not_implemented #()

  let read_stream_or_cancel_exn _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let read_stream_nonblocking _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let write_stream_or_cancel_exn _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let write_stream_nonblocking _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let wait_or_cancel_exn _ = not_implemented #()

  let read_seekable_or_cancel_exn _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let write_seekable_or_cancel_exn _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let resize _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let flush _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let%template stat _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let current_dir _ = not_implemented #()
  let open_dir_at _ = not_implemented #()

  let%template read_dir_at _ = not_implemented #()
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let%template stat_at _ = not_implemented #()
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let create_dir_at _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let remove_dir_at _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let remove_file_at _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let rename_at _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let create_link_at _ =
    match not_implemented #() with
    | (_ : Nothing.t) -> .
  ;;

  let%template read_link_at _ = not_implemented #()
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let open_file_to_read_at _ = not_implemented #()
  let open_or_create_file_at _ = not_implemented #()
  let open_or_create_file_to_append_at _ = not_implemented #()
end

let create await = exclave_ Io.Expert.create await (module Io_prim) ()
