open Core
open Await
module Htbl = Portable_lockfree_htbl

module Raw = struct
  external errno : (int[@untagged]) -> (int[@untagged]) @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_errno_noalloc"
  [@@noalloc]

  external raise_unix_error : int -> string -> 'a @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_raise_unix_error"

  external create : unit -> (int[@untagged]) @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_create_noalloc"
  [@@noalloc]

  external wakeup : (int[@untagged]) -> (int[@untagged]) @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_wakeup_noalloc"
  [@@noalloc]

  external add
    :  (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_add_noalloc"
  [@@noalloc]

  external del
    :  (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    -> (int[@untagged])
    @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_del_noalloc"
  [@@noalloc]

  external wait : int -> int array -> int @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_wait"

  external poll : (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_poll_noalloc"
  [@@noalloc]

  external set_nosigpipe : (int[@untagged]) -> (int[@untagged]) @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_set_nosigpipe_noalloc"
  [@@noalloc]

  external set_nonblock : (int[@untagged]) -> (int[@untagged]) @@ portable
    = "caml_no_bytecode_impl" "ocaml_io_kq_set_nonblock_noalloc"
  [@@noalloc]
end

module Syscall = struct
  let eagain = Raw.errno 0
  let eintr = Raw.errno 1
  let einprogress = Raw.errno 2
  let eexist = Raw.errno 3
  let enoent = Raw.errno 4
  let eisdir = Raw.errno 5
  let[@inline never] raise_unix_error errno ~name = Raw.raise_unix_error errno name
  let set_nosigpipe fd = Raw.set_nosigpipe (Core_unix.File_descr.to_int fd)
  let set_nonblock fd = Raw.set_nonblock (Core_unix.File_descr.to_int fd)
end

module Filters = struct
  type t = int

  (* These must match the [IO_KQ_*] constants in the stubs. *)
  let in_ = 1
  let out = 2
  let in_out = 3
  let err = 4
  let mem t flags = t land flags <> 0
end

module Id : sig @@ portable
  (** Ids of waiters, used as the [udata] of knotes. Ids are non-negative, which leaves
      negative [udata] values for internal use. *)

  type t : immediate

  val random : unit -> t
  val equal : t @ contended -> t @ contended -> bool
  val hash : t @ contended -> int
  val to_int : t -> int
  val of_int : int -> t
end = struct
  type t = int

  let random () = Int64.to_int_trunc (Random.bits64 ()) land Int.max_value
  let equal = ( = )
  let hash = Fn.id
  let to_int = Fn.id
  let of_int = Fn.id
end

module Awaiter = struct
  (** State of a single wait.

      [state] is [pending] until either the poller reports readiness (and stores the
      ready filters, which are positive) or the wait is canceled (and stores [canceled]).
      Whoever changes the state from [pending] signals the [trigger]. *)
  type t =
    { mutable state : int [@atomic]
    ; trigger : Trigger.Source.t
    ; fd : int
    ; filters : int
    }

  let pending = 0
  let canceled = -1

  let try_complete t result =
    match
      Atomic.Loc.compare_and_set
        [%atomic.loc t.state]
        ~if_phys_equal_to:pending
        ~replace_with:result
    with
    | Set_here ->
      Trigger.Source.signal t.trigger;
      true
    | Compare_failed -> false
  ;;
end

module Awaiters : sig @@ portable
  type t : value mod contended portable unyielding

  val create : unit -> t
  val add : t @ local -> Awaiter.t -> Id.t
  val find : t @ local -> Id.t -> Awaiter.t or_null @ contended
  val remove : t @ local -> Id.t -> unit
  val is_empty : t @ local -> bool
end = struct
  type t = (Id.t, Awaiter.t) Htbl.t

  let create () = Htbl.create (module Id)

  let rec add t awaiter =
    let id = Id.random () in
    match Htbl.add t ~key:id ~data:awaiter with
    | Null -> id
    | This _ -> add t awaiter
  ;;

  let find t id = Htbl.find t id

  let remove t id =
    match Htbl.remove t id with
    | Null -> ()
    | This _ -> ()
  ;;

  let is_empty t = Htbl.non_linearizable_length t = 0
end

type t =
  { kq : int
  ; awaiters : Awaiters.t @@ global
  ; closed : bool Atomic.t @@ global
  }

let wakeup_udata = -1
let max_events = 128

let poller_main { kq; awaiters; closed } =
  let out = Array.create ~len:(2 * max_events) 0 in
  let rec loop () =
    let n = Raw.wait kq out in
    if n < 0
    then (
      if n <> -Syscall.eintr
      then (
        (* This should only happen in case of bugs, e.g. the kqueue was closed. *)
        eprintf "Io_in_kqueue: kevent failed (errno %d), poller exiting\n%!" (-n);
        Atomic.set closed true))
    else
      for i = 0 to n - 1 do
        let udata = Array.unsafe_get out (2 * i) in
        if udata <> wakeup_udata
        then (
          match Awaiters.find awaiters (Id.of_int udata) with
          | Null -> ( (* The wait has already been canceled and removed. *) )
          | This awaiter ->
            let result = Array.unsafe_get out ((2 * i) + 1) in
            (* On error or end-of-file we report the requested filters as ready, so that
               the caller performs the operation and observes the condition. *)
            let result =
              if result land Filters.in_out = 0 then result lor awaiter.filters else result
            in
            ignore (Awaiter.try_complete awaiter result : bool))
      done;
    if not (Atomic.get closed) then loop ()
  in
  (try loop () with
   | exn ->
     (* We should not be getting exceptions except in case of bugs, in which case pending
        and future waits will never complete. As with the [io_uring] consumer thread, we
        print the exception and fall through. *)
     eprintf "Io_in_kqueue: poller exiting due to: %s\n%!" (Exn.to_string exn));
  Core_unix.close (Core_unix.File_descr.of_int kq)
;;

let create () =
  let kq = Raw.create () in
  if kq < 0 then Syscall.raise_unix_error (-kq) ~name:"kqueue";
  let t = { kq; awaiters = Awaiters.create (); closed = Atomic.make false } in
  match
    Multicore.spawn_on
      ~domain:(Multicore.current_domain ())
      (fun { aliased_many = t } -> poller_main t)
      { aliased_many = t }
  with
  | Spawned -> t
  | Failed (_, exn, bt) ->
    Core_unix.close (Core_unix.File_descr.of_int kq);
    Exn.raise_with_original_backtrace exn bt
;;

let close t =
  if not (Atomic.exchange t.closed true)
  then (
    let r = Raw.wakeup t.kq in
    if r < 0 then Syscall.raise_unix_error (-r) ~name:"kevent")
;;

let poll fd filters =
  let r = Raw.poll (Core_unix.File_descr.to_int fd) filters in
  if r < 0 then Syscall.raise_unix_error (-r) ~name:"poll" else r
;;

(* Called (at most once) by the terminator or cancellation token when the wait is
   terminated or canceled. This runs in an unknown context and must not block or raise.

   The callback is responsible for removing the awaiter from the table, see
   [wait_or_cancel_exn]. *)
let terminate_or_cancel ~kq ~awaiters id =
  (match Awaiters.find awaiters id with
   | Null -> ()
   | This awaiter ->
     (match
        Atomic.Loc.compare_and_set
          [%atomic.loc awaiter.state]
          ~if_phys_equal_to:Awaiter.pending
          ~replace_with:Awaiter.canceled
      with
      | Compare_failed -> ( (* Readiness was reported first. *) )
      | Set_here ->
        let (_ : int) = Raw.del kq awaiter.fd awaiter.filters (Id.to_int id) in
        Trigger.Source.signal awaiter.trigger));
  Awaiters.remove awaiters id
;;

let[@inline never] raise_terminated_or_canceled await =
  if Terminator.is_terminated (Await.terminator await)
  then raise Await.Terminated
  else raise_notrace Or_canceled.Exn.Canceled
;;

let wait_or_cancel_exn await cancel { kq; awaiters; closed = _ } fd filters =
  let fd = Core_unix.File_descr.to_int fd in
  let trigger = Trigger.create () in
  let awaiter =
    { Awaiter.state = Awaiter.pending; trigger = Trigger.source trigger; fd; filters }
  in
  let id = Awaiters.add awaiters awaiter in
  let r = Raw.add kq fd filters (Id.to_int id) in
  if r < 0
  then (
    Awaiters.remove awaiters id;
    (* The trigger must be used before it can be dropped. *)
    Trigger.Source.signal (Trigger.source trigger);
    Syscall.raise_unix_error (-r) ~name:"kevent");
  (match
     Await.await_with_terminate_or_cancel
       await
       cancel
       trigger
       ~terminate_or_cancel:(fun id -> terminate_or_cancel ~kq ~awaiters id)
       id
   with
   | Dropped ->
     (* [terminate_or_cancel] won't be called, so we must remove the awaiter. *)
     Awaiters.remove awaiters id
   | Already_signalled ->
     ( (* [terminate_or_cancel] was or will be called and removes the awaiter. *) ));
  let state = Atomic.Loc.get [%atomic.loc awaiter.state] in
  if state = Awaiter.canceled
  then raise_terminated_or_canceled await
  else (
    if filters = Filters.in_out
    then
      (* Only one of the two one-shot knotes might have fired, so delete the other one to
         avoid leaving it registered. *)
      ignore (Raw.del kq fd filters (Id.to_int id) : int);
    state)
;;

module For_testing = struct
  let appears_idle t = Awaiters.is_empty t.awaiters
end
