(* Tests for the kqueue based [Io.t] implementation.

   These exercise the [Io.Expert] interface, i.e. exactly the surface implemented by
   [Io_in_kqueue], using systhreads for concurrency. *)

open Core
open Ox
open Await
open Io_kernel
module Thread = Caml_threads.Thread
open Filesystem_types
open Io_types
module Io_descr = Io_descr_native
module Io_result = Io.Expert.Io_result

let poller = Io_in_kqueue.create ()

let check name cond =
  if not cond then failwithf "FAILED: %s" name ();
  printf "ok: %s\n%!" name
;;

(* Runs [f] with an [Io.t] using a blocking await, i.e. suspending the calling thread. *)
let with_io ?(terminator = Terminator.unkillable) f =
  let await = (Await.Expert.create [@alloc stack]) ~sync:Sync.blocking ~terminator in
  let result = ref None in
  Io_in_kqueue.with_io await poller ~f:(fun io -> result := Some (f io));
  Option.value_exn !result
;;

let in_thread f = Thread.create (fun () -> f ()) ()

let read (io : Io.t @ local) ?(cancel = Cancellation.never) (fd : Io_descr.t @ local) (buf : bytes) =
  let n =
    Io.Expert.read_stream_or_cancel_exn #(io, cancel, fd, Slice.of_bytes buf)
    |> Io_result.to_isize_non_negative_unsafe
  in
  Isize.to_int_trunc n
;;

let write (io : Io.t @ local) (fd : Io_descr.t @ local) (s : string) =
  let buf = Bytes.of_string s in
  let rec loop pos =
    if pos < Bytes.length buf
    then (
      let n =
        Io.Expert.write_stream_or_cancel_exn
          #( io
           , Cancellation.never
           , fd
           , { modal = (Slice.sub [@kind bits8]) (Slice.of_bytes buf) ~pos:(I64.of_int pos) ~len:(I64.of_int (Bytes.length buf - pos)) } )
        |> Io_result.to_isize_non_negative_unsafe
        |> Isize.to_int_trunc
      in
      loop (pos + n))
  in
  loop 0 [@nontail]
;;

let read_all (io : Io.t @ local) (fd : Io_descr.t @ local) =
  let out = Buffer.create 1024 in
  let buf = Bytes.create 65536 in
  let rec loop () =
    match read io fd buf with
    | 0 -> Buffer.contents out
    | n ->
      Buffer.add_subbytes out buf ~pos:0 ~len:n;
      loop ()
  in
  loop () [@nontail]
;;

let stream_fd ?(roles = Io_descr.Roles.(readable + writable)) raw =
  Io_descr.create roles ~shutdown:false raw
;;

(* Pipes are blocking descriptors (like stdin): reads must wait in the kqueue rather than
   block the thread, and EOF must be reported. *)
let test_pipe () =
  with_io (fun io ->
    let r, w = Core_unix.pipe () in
    let rfd = stream_fd ~roles:Io_descr.Roles.readable r in
    let writer =
      in_thread (fun () ->
        Thread.delay 0.1;
        ignore (Core_unix.write_substring w ~buf:"hello" : int);
        Thread.delay 0.1;
        Core_unix.close w)
    in
    let start = Time_float.now () in
    let buf = Bytes.create 16 in
    let n = read io rfd buf in
    let waited = Time_float.diff (Time_float.now ()) start in
    check "pipe: read waits for data" (n = 5 && Time_float.Span.(waited >= of_sec 0.05));
    check "pipe: data" (String.equal (Bytes.To_string.sub buf ~pos:0 ~len:n) "hello");
    check "pipe: eof" (read io rfd buf = 0);
    Thread.join writer;
    let #() = Io.Expert.close_readable #(io, rfd) in
    ())
;;

let test_cancel_and_terminate () =
  with_io (fun io ->
    let r, w = Core_unix.pipe () in
    let rfd = stream_fd ~roles:Io_descr.Roles.readable r in
    let canceled =
      Cancellation.with_ (fun cancel ->
        let source = Cancellation.source cancel in
        let canceller =
          match source with
          | Null -> assert false
          | This source ->
            in_thread (fun () ->
              Thread.delay 0.1;
              Cancellation.Source.cancel source)
        in
        let result =
          match read io ~cancel rfd (Bytes.create 16) with
          | _ -> false
          | exception Or_canceled.Exn.Canceled -> true
        in
        Thread.join canceller;
        result)
    in
    check "cancel: pending read is canceled" canceled;
    check "cancel: no waiters left" (Io_in_kqueue.Kqueue_poller.For_testing.appears_idle poller);
    (* The descriptor is still usable after cancellation. *)
    ignore (Core_unix.write_substring w ~buf:"x" : int);
    check "cancel: read after cancel" (read io rfd (Bytes.create 16) = 1);
    Core_unix.close w;
    (let #() = Io.Expert.close_readable #(io, rfd) in ()));
  let r, w = Core_unix.pipe () in
  let terminated =
    Terminator.with_ (fun terminator ->
      let source =
        match Terminator.source terminator with
        | Null -> assert false
        | This source -> source
      in
      let terminate =
        in_thread (fun () ->
          Thread.delay 0.1;
          Terminator.Source.terminate source)
      in
      let result =
        with_io ~terminator (fun io ->
          let rfd = stream_fd ~roles:Io_descr.Roles.readable r in
          match read io rfd (Bytes.create 16) with
          | _ -> false
          | exception Await.Terminated -> true)
      in
      Thread.join terminate;
      result)
  in
  check "terminate: pending read is terminated" terminated;
  check "terminate: no waiters left" (Io_in_kqueue.Kqueue_poller.For_testing.appears_idle poller);
  Core_unix.close r;
  Core_unix.close w
;;

let temp_dir () =
  let dir = Filename_unix.temp_dir "io_in_kqueue" "" in
  Core_unix.openfile dir ~mode:[ O_RDONLY ], dir
;;

let path = File_path.of_string

let test_files () =
  with_io (fun io ->
    let dir_raw, dir_name = temp_dir () in
    let dir = Io_descr.create Io_descr.Roles.readable ~shutdown:false dir_raw in
    let fd =
      Io.Expert.open_or_create_file_at
        #( io
         , ~create:(Always File_permissions.u_rw)
         , ~relative_to:dir
         , ~follow_link:true
         , path "file" )
    in
    let data = String.init 100_000 ~f:(fun i -> Char.of_int_exn (i % 256)) in
    let n =
      Io.Expert.write_seekable_or_cancel_exn
        #( io
         , Cancellation.never
         , fd
         , ~offset:(I64.of_int 0)
         , { modal = Slice.of_bytes (Bytes.of_string data) } )
    in
    check "file: pwrite" (Isize.to_int_trunc n = 100_000);
    let stats : File_stats.t = Io.Expert.stat #(io, fd) in
    check "file: fstat size" (Int64.equal (Int64_u.to_int64 stats.size) 100_000L);
    (* Read through a bigstring, i.e. directly into external memory. *)
    let bs = Bigstring.create 10 in
    let n =
      (Slice.with_bigstring [@kind word]) bs ~f:(fun slice ->
        Io.Expert.read_seekable_or_cancel_exn
          #(io, Cancellation.never, fd, ~offset:(I64.of_int 1000), slice))
    in
    check
      "file: pread into bigstring"
      (Isize.to_int_trunc n = 10
       && String.equal (Bigstring.to_string bs) (String.sub data ~pos:1000 ~len:10));
    (* A regular file at EOF is "ready" (kqueue alone would never report it). *)
    let flags = Io.Expert.wait_or_cancel_exn #(io, Cancellation.never, fd, Io_wait_flags.in_) in
    check "file: wait on regular file" (Io_wait_flags.(do_intersect in_) flags);
    let #() = Io.Expert.resize #(io, fd, I64.of_int 10) in
    let #() = Io.Expert.flush #(io, fd) in
    check "file: resize" (Int64.equal (Int64_u.to_int64 (Io.Expert.stat #(io, fd)).size) 10L);
    let #() = Io.Expert.close_bidirect #(io, fd) in
    (* Appending is a stream write on a regular file. *)
    let afd =
      Io.Expert.open_or_create_file_to_append_at
        #(io, ~create:Never, ~relative_to:dir, ~follow_link:true, path "file")
    in
    write io afd "appended";
    let #() = Io.Expert.close_writable #(io, afd) in
    let rfd =
      Io.Expert.open_file_to_read_at
        #(io, ~relative_to:dir, ~follow_link:true, path "file")
    in
    let contents = read_all io rfd in
    check
      "file: append + stream read to eof"
      (String.equal contents (String.sub data ~pos:0 ~len:10 ^ "appended"));
    let #() = Io.Expert.close_bidirect #(io, rfd) in
    (* File system operations. *)
    let #() =
      Io.Expert.create_dir_at
      #(io, ~relative_to:dir, path "a/b/c", ~parents:true, File_permissions.u_rwx)
    in
    let #() =
      Io.Expert.create_dir_at
      #(io, ~relative_to:dir, path "a/b/c", ~parents:true, File_permissions.u_rwx)
    in
    check
      "fs: mkdir -p (idempotent)"
      (match
         (Io.Expert.stat_at #(io, ~relative_to:dir, ~follow_link:true, path "a/b/c") : File_stats.t).kind
       with
       | Directory -> true
       | _ -> false);
    (match
       Io.Expert.create_dir_at
         #(io, ~relative_to:dir, path "file/x", ~parents:true, File_permissions.u_rwx)
     with
     | #() -> check "fs: mkdir -p under a file fails" false
     | exception Core_unix.Unix_error (ENOTDIR, _, _) ->
       check "fs: mkdir -p under a file fails" true);
    let #() = Io.Expert.create_link_at #(io, ~into:"file", ~relative_to:dir, path "link") in
    check
      "fs: symlink + readlink"
      (String.equal (Io.Expert.read_link_at #(io, ~relative_to:dir, path "link")) "file");
    check
      "fs: lstat symlink"
      (match
         (Io.Expert.stat_at #(io, ~relative_to:dir, ~follow_link:false, path "link") : File_stats.t).kind
       with
       | Symlink -> true
       | _ -> false);
    (match
       Io.Expert.open_file_to_read_at
         #(io, ~relative_to:dir, ~follow_link:false, path "link")
     with
     | (_ : Io_descr.t) -> check "fs: nofollow open of symlink fails" false
     | exception Core_unix.Unix_error (ELOOP, _, _) ->
       check "fs: nofollow open of symlink fails" true);
    let #() =
      Io.Expert.rename_at
      #( io
       , ~relative_to_before:dir
       , ~before:(path "file")
       , ~relative_to_after:dir
       , ~after:(path "renamed") )
    in
    let entries =
      Io.Expert.read_dir_at #(io, ~relative_to:dir, ~follow_link:true, path ".")
      |> Iarray.to_list
      |> List.map ~f:File_path.Part.to_string
      |> List.sort ~compare:String.compare
    in
    check
      "fs: rename + read_dir"
      ([%equal: string list] entries [ "a"; "link"; "renamed" ]);
    let sub = Io.Expert.open_dir_at #(io, ~relative_to:dir, ~follow_link:true, path "a") in
    let #() = Io.Expert.remove_dir_at #(io, ~relative_to:sub, path "b/c") in
    let #() = Io.Expert.remove_dir_at #(io, ~relative_to:sub, path "b") in
    let #() = Io.Expert.close_readable #(io, sub) in
    let #() = Io.Expert.remove_dir_at #(io, ~relative_to:dir, path "a") in
    let #() = Io.Expert.remove_file_at #(io, ~relative_to:dir, path "link") in
    let #() = Io.Expert.remove_file_at #(io, ~relative_to:dir, path "renamed") in
    (match
       Io.Expert.open_file_to_read_at
         #(io, ~relative_to:dir, ~follow_link:true, path "renamed")
     with
     | (_ : Io_descr.t) -> check "fs: open missing file fails" false
     | exception Core_unix.Unix_error (ENOENT, _, _) ->
       check "fs: open missing file fails" true);
    let #() = Io.Expert.close_readable #(io, dir) in
    Core_unix.rmdir dir_name)
;;

let localhost_any_port =
  Sockaddr_unix.of_unix_sockaddr_exn (ADDR_INET (Core_unix.Inet_addr.localhost, 0))
;;

(* Several servers accept on the same listener concurrently (i.e. several waiters on one
   descriptor), clients send large amounts of data in both directions so that writes
   fill the socket buffers and have to wait for writability. *)
let test_tcp () =
  let num_clients = 8 in
  let size = 1 lsl 20 in
  let payload i = String.make size (Char.of_int_exn (Char.to_int 'a' + i)) in
  let listener =
    with_io (fun io ->
      Io.Expert.listen_or_cancel_exn
        #( io
         , Cancellation.never
         , ~opts:{ Socket_opts.default with reuse_addr = This true }
         , ~max_pending_requests:64
         , localhost_any_port ))
  in
  let addr = with_io (fun io -> Io.Expert.addr #(io, listener)) in
  let results = Array.create ~len:num_clients false in
  let servers =
    List.init num_clients ~f:(fun _ ->
      in_thread (fun () ->
        with_io (fun io ->
          let #(~fd, ~client_addr:_) =
            Io.Expert.accept_or_cancel_exn
              #(io, Cancellation.never, listener, ~opts:Socket_opts.default)
          in
          (* Echo everything back. *)
          let buf = Bytes.create 65536 in
          let rec loop () =
            match read io fd buf with
            | 0 -> ()
            | n ->
              write io fd (Bytes.To_string.sub buf ~pos:0 ~len:n);
              loop ()
          in
          loop ();
          let #() = Io.Expert.close_bidirect #(io, fd) in
          ())))
  in
  let clients =
    List.init num_clients ~f:(fun i ->
      in_thread (fun () ->
        with_io (fun io ->
          let fd =
            Io.Expert.connect_or_cancel_exn
              #(io, Cancellation.never, ~opts:{ Socket_opts.default with nodelay = This true }, addr)
          in
          let reader =
            in_thread (fun () ->
              with_io (fun io ->
                let echoed = read_all io fd in
                results.(i) <- String.equal echoed (payload i)))
          in
          write io fd (payload i);
          let #() = Io.Expert.close_writable #(io, fd) in
          Thread.join reader;
          let #() = Io.Expert.close_readable #(io, fd) in
          ())))
  in
  List.iter clients ~f:Thread.join;
  List.iter servers ~f:Thread.join;
  with_io (fun io ->
    let #() = Io.Expert.close_readable #(io, listener) in
    ());
  check
    (sprintf "tcp: %d concurrent echo connections of %d bytes" num_clients size)
    (Array.for_all results ~f:Fn.id);
  check "tcp: no waiters left" (Io_in_kqueue.Kqueue_poller.For_testing.appears_idle poller)
;;

let test_udp () =
  with_io (fun io ->
    let bind () =
      Io.Expert.bind_datagram_socket_or_cancel_exn
        #(io, Cancellation.never, ~opts:Socket_opts.default, ~bind_to:localhost_any_port)
    in
    let a = bind () in
    let b = bind () in
    let a_addr = Io.Expert.addr #(io, a) in
    let b_addr = Io.Expert.addr #(io, b) in
    let sender =
      in_thread (fun () ->
        with_io (fun io ->
          Thread.delay 0.1;
          let n =
            Io.Expert.send_to_or_cancel_exn
              #( io
               , Cancellation.never
               , b
               , { modal = Slice.of_bytes (Bytes.of_string "datagram") }
               , a_addr )
          in
          assert (Isize.to_int_trunc n = 8)))
    in
    let buf = Bytes.create 64 in
    let #(~len, ~sender_addr) =
      Io.Expert.recv_from_or_cancel_exn #(io, Cancellation.never, a, Slice.of_bytes buf)
    in
    Thread.join sender;
    check
      "udp: recv_from waits and reports sender"
      (String.equal (Bytes.To_string.sub buf ~pos:0 ~len:(Isize.to_int_trunc len)) "datagram"
       && Sockaddr.equal sender_addr b_addr);
    let #(~len, ~sender_addr) =
      Io.Expert.recv_from_nonblocking #(io, a, Slice.of_bytes buf)
    in
    check
      "udp: recv_from_nonblocking would block"
      (Or_null.is_null sender_addr
       && match Io_result.to_or_would_block len with
          | Would_block -> true
          | _ -> false);
    let #() = Io.Expert.close_bidirect #(io, a) in
    let #() = Io.Expert.close_bidirect #(io, b) in
    ())
;;

(* macOS has no [MSG_NOSIGNAL]: writing to a connection closed by the peer must raise
   [EPIPE] (or [ECONNRESET]) rather than kill the process with [SIGPIPE]. *)
let test_sigpipe () =
  with_io (fun io ->
    let listener =
      Io.Expert.listen_or_cancel_exn
        #( io
         , Cancellation.never
         , ~opts:Socket_opts.default
         , ~max_pending_requests:1
         , localhost_any_port )
    in
    let addr = Io.Expert.addr #(io, listener) in
    let client =
      Io.Expert.connect_or_cancel_exn
        #(io, Cancellation.never, ~opts:Socket_opts.default, addr)
    in
    let #(~fd:server, ~client_addr:_) =
      Io.Expert.accept_or_cancel_exn
        #(io, Cancellation.never, listener, ~opts:Socket_opts.default)
    in
    let #() = Io.Expert.close_bidirect #(io, server) in
    let raised =
      match
        for _ = 1 to 100 do
          write io client (String.make 65536 'x')
        done
      with
      | () -> false
      | exception Core_unix.Unix_error ((EPIPE | ECONNRESET), _, _) -> true
    in
    check "tcp: write to closed peer raises EPIPE (no SIGPIPE)" raised;
    let #() = Io.Expert.close_bidirect #(io, client) in
    let #() = Io.Expert.close_readable #(io, listener) in
    ())
;;

(* Many threads ping-pong over pipes through the same poller. Lost wake-ups would hang. *)
let test_stress () =
  let num_pairs = 32 in
  let rounds = 2_000 in
  let start = Time_float.now () in
  let threads =
    List.init num_pairs ~f:(fun _ ->
      let r1, w1 = Core_unix.pipe () in
      let r2, w2 = Core_unix.pipe () in
      let ping =
        in_thread (fun () ->
          with_io (fun io ->
            let r = stream_fd ~roles:Io_descr.Roles.readable r2 in
            let w = stream_fd ~roles:Io_descr.Roles.writable w1 in
            let buf = Bytes.create 1 in
            for _ = 1 to rounds do
              write io w "x";
              assert (read io r buf = 1)
            done;
            let #() = Io.Expert.close_writable #(io, w) in
            let #() = Io.Expert.close_readable #(io, r) in
            ()))
      in
      let pong =
        in_thread (fun () ->
          with_io (fun io ->
            let r = stream_fd ~roles:Io_descr.Roles.readable r1 in
            let w = stream_fd ~roles:Io_descr.Roles.writable w2 in
            let buf = Bytes.create 1 in
            for _ = 1 to rounds do
              assert (read io r buf = 1);
              write io w "y"
            done;
            let #() = Io.Expert.close_writable #(io, w) in
            let #() = Io.Expert.close_readable #(io, r) in
            ()))
      in
      [ ping; pong ])
    |> List.concat
  in
  List.iter threads ~f:Thread.join;
  let elapsed = Time_float.diff (Time_float.now ()) start in
  check
    (sprintf
       "stress: %d ping-pong pairs x %d rounds (%.0f round trips/s)"
       num_pairs
       rounds
       (Float.of_int (num_pairs * rounds) /. Time_float.Span.to_sec elapsed))
    true;
  check "stress: no waiters left" (Io_in_kqueue.Kqueue_poller.For_testing.appears_idle poller)
;;

let () =
  test_pipe ();
  test_cancel_and_terminate ();
  test_files ();
  test_tcp ();
  test_udp ();
  test_sigpipe ();
  test_stress ();
  Io_in_kqueue.close poller;
  printf "all tests passed\n%!"
;;
