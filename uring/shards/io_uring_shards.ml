open Linux_ext
open Core
open Ox
open Await
module Domain = Basement.Stdlib_shim.Domain
module Htbl = Portable_lockfree_htbl
module Uring = Io_uring_bindings

(* *)

module Id : sig @@ portable
  (** A way to generate ids, to be used as tags for ordinary requests, and special tag for
      cancel requests. *)

  type t : immediate

  val random : unit -> t
  val equal : t @ contended -> t @ contended -> bool
  val hash : t @ contended -> int
  val of_tag : Uring.Tag.t -> t
  val to_tag : t -> Uring.Tag.t
  val cancel_tag : t -> Uring.Tag.t
  val is_cancel_tag : Uring.Tag.t -> bool
end = struct
  type t = int

  let random () = Int64.to_int_trunc (Random.bits64 ()) land Int.max_value
  let equal = ( = )
  let hash = Fn.id
  let of_tag tag = (Isize.to_int_trunc tag lsl 1) lsr 1
  let to_tag = Isize.of_int
  let () = assert%debug (Isize.O.(Isize.max_value > Isize.of_int Int.max_value))

  let cancel_tag id =
    (* We use the sign bit to distinguish cancellations from other requests. *)
    Isize.O.(Isize.min_value lor to_tag id)
  ;;

  let is_cancel_tag = Isize.is_negative
end

module Awaiter : sig @@ portable
  (** Result cell, to be filled, and trigger, to be signaled, once a call is fully
      completed. *)

  type t : value non_float mod contended portable

  val make : completions:int -> cancellable:bool -> Trigger.Source.t -> t

  module Died_or_left_alive : sig
    type t =
      | Left_alive
      | Died
  end

  val no_longer_cancellable : t -> Died_or_left_alive.t
  val cancelled : t -> Uring.Result.t -> Died_or_left_alive.t
  val completed : t -> Uring.Result.t -> Died_or_left_alive.t
  val get : t -> Uring.Result.t
  val is_dead : t -> bool
end = struct
  type t =
    { mutable reference_count : int [@atomic]
    ; mutable result : Uring.Result.t [@atomic]
    ; trigger : Trigger.Source.t
    }

  let cancellable_bit = 1 (* This is assumed to be [1] in [make]. *)
  let completions_one = 2

  (* The [reference_count] has the following layout:

     {v
       Bit: [      0      |           1 ..            ]
       Use: [ cancellable | completions being awaited ]
     v} *)

  let make ~completions ~cancellable trigger =
    { reference_count = (completions * completions_one) + Bool.to_int cancellable
    ; result = Uring.Result.create_ok 0 (* [Ok 0] is considered least interesting. *)
    ; trigger
    }
  ;;

  module Died_or_left_alive = struct
    type t =
      | Left_alive (* [false] *)
      | Died (* [true] *)
    [@@deriving compare ~localize, sexp_of]

    external of_died : bool -> t @@ portable = "%identity"

    let%test_unit "of_died" =
      [%test_eq: t] (of_died false) Left_alive;
      [%test_eq: t] (of_died true) Died
    ;;
  end

  let no_longer_cancellable t =
    let prior_reference_count =
      Atomic.Loc.fetch_and_add [%atomic.loc t.reference_count] (-cancellable_bit)
    in
    assert%debug (
      prior_reference_count > 0 && prior_reference_count land cancellable_bit <> 0);
    Died_or_left_alive.of_died (prior_reference_count = cancellable_bit)
  ;;

  let cancelled t result =
    (* Cancellation may fail for a couple of valid reasons, see [io_uring_prep_cancel(3)].
       If it fails for other reasons, e.g. [EINVAL], it might indicate a bug, which is why
       we assert the result, but we do not really care about the result of the
       cancellation request itself, because, when cancellation fails, it doesn't really
       matter (unless it indicates a bug), and cancellation is generally triggered from
       contexts where there is no handler for the error. *)
    assert%debug (
      Uring.Result.is_ok result
      || Uring.Result.is_noent result
      || Uring.Result.is_already result);
    no_longer_cancellable t
  ;;

  let[@inline] completed t result =
    let[@inline] rec update_result () =
      let before = Atomic.Loc.get [%atomic.loc t.result] in
      if Uring.Result.is_more_specific result ~than:before
      then (
        match
          Atomic.Loc.compare_and_set
            [%atomic.loc t.result]
            ~if_phys_equal_to:before
            ~replace_with:result
        with
        | Set_here -> ()
        | Compare_failed -> update_result ())
    in
    update_result ();
    let prior_reference_count =
      Atomic.Loc.fetch_and_add [%atomic.loc t.reference_count] (-completions_one)
    in
    assert%debug (prior_reference_count >= completions_one);
    if prior_reference_count <= completions_one + cancellable_bit
    then Trigger.Source.signal t.trigger;
    Died_or_left_alive.of_died (prior_reference_count = completions_one)
  ;;

  let is_dead t = t.reference_count = 0

  let get t =
    assert%debug (Trigger.Source.is_signalled t.trigger);
    t.result
  ;;
end

module Awaiters : sig @@ portable
  (** Table indexed by {!Id}s for awaiters to store the result to and signal on
      completion. *)

  type t : value mod contended portable unyielding

  val create : unit -> t
  val add : t @ local -> Awaiter.t -> Id.t
  val find : t @ local -> Id.t -> Awaiter.t or_null @ contended
  val remove : t @ local -> Id.t -> unit

  module For_testing : sig
    (** [is_empty t] is only reliable when the awaiters table is not being modified
        concurrently. The intended use case is to check that no awaiters have been leaked
        at the end of test/program. *)
    val is_empty : t @ local -> bool
  end
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
    | Null -> assert%debug false
    | This awaiter -> assert%debug (Awaiter.is_dead awaiter)
  ;;

  module For_testing = struct
    let is_empty t = Htbl.non_linearizable_length t = 0
  end
end

module State_of_a_ring : sig @@ portable
  type t : immediate

  val initial : ring_rc:int -> t

  (* *)

  val ring_rc : t -> int
  val and_decr_ring_rc : t -> t
  val fetch_and_incr_ring_rc : t Atomic.Loc.t @ local -> t
  val fetch_and_decr_ring_rc : t Atomic.Loc.t @ local -> t

  (* *)

  val eventfd_rc : t -> int
  val fetch_and_decr_eventfd_rc : t Atomic.Loc.t @ local -> t

  (* *)

  val is_live : t -> bool
  val and_kill : t -> t
end = struct
  (* The state consists of the ring reference count, the eventfd reference count, and a
     live bit.

     {v
       Bit: [   0  |    1 - 2   | 3 - n-1 ]
       Use: [ live | eventfd rc | ring rc ]
     v}

     The live bit starts as [1] and once it is cleared to [0] it means the ring is being
     destroyed and must no longer be used. The party/operation that clears the live bit is
     responsible for ensuring that the consumer thread is woken up to kill the ring.

     The ring reference count is intentionally separate from the live bit. It is not
     invalid to have a positive ring reference count and a [0] live bit. This allows the
     reference count to be incremented using [fetch_and_add], which is simpler and faster
     than using a retry loop using [compare_and_set]. Once incremented, the reference
     count can also be decremented using [fetch_and_add].

     The eventfd reference count starts at 2, accounting for both the consumer thread and
     submission side, and is only decremented to determine who must close the eventfd
     registered to the ring.

     Fibers can move between domains and every operation on a ring has to temporarily
     increment the reference count of the ring for the current domain. This means that
     those increments and decrements should ideally be as fast as possible. Other
     alternatives such as e.g. requiring a ring to exist for every domain or always having
     some amount of rings per set (and then having each op pick one of those rings) also
     have their problems. *)

  type t = int

  let ring_shift = 3
  let ring_one = 1 lsl ring_shift
  let eventfd_shift = 1
  let eventfd_one = 1 lsl eventfd_shift
  let live_bit = 1

  (* *)

  let initial ~ring_rc =
    (* The fields are disjoint, so we can use [+] instead of [lor]. On x86 this can
       potentially be translated to a single [LEA] instruction. *)
    (ring_rc * ring_one) + (2 * eventfd_one) + live_bit
  ;;

  (* *)

  let ring_rc t = t asr ring_shift
  let and_decr_ring_rc t = t - ring_one
  let fetch_and_incr_ring_rc t = Atomic.Loc.fetch_and_add t ring_one
  let fetch_and_decr_ring_rc t = Atomic.Loc.fetch_and_add t (-ring_one)

  (* *)

  let eventfd_rc t = (t asr eventfd_shift) land 3
  let fetch_and_decr_eventfd_rc t = Atomic.Loc.fetch_and_add t (-eventfd_one)

  (* *)

  let is_live t = t land live_bit <> 0
  let and_kill t = t land lnot live_bit
end

(* For particular ring slot in a set of rings we might have a [Ring], [No_ring], or we
   might have [No_ring_pinned]. The idea is that pinning increments the reference counts,
   forcing existing rings to remain alive, but does not create new rings. A [Ring] has a
   separate reference count as a part of its state. [No_ring] corresponds to a reference
   count of [0]. [No_ring_pinned] corresponds to a reference count of [1]. *)

type _ ring_or_no_ring =
  | Ring :
      { mutable state : State_of_a_ring.t [@atomic]
      ; mutable sq_mutex : 'sq Sync.Mutex.Loc.State.t [@atomic]
      ; sq : (Uring.Sq.t, 'sq) Capsule.Prim.Data.t
      ; mutable cq_mutex : 'cq Sync.Mutex.Loc.State.t [@atomic]
      ; cq : (Uring.Cq.t, 'cq) Capsule.Prim.Data.t
      ; cq_consumer_eventfd : Eventfd.t
      ; mutable cancellations : Id.t list [@atomic]
      ; terminate_or_cancel : Id.t @ contended once -> unit @@ portable
      }
      -> [> `Ring ] ring_or_no_ring
  | No_ring : [> `No_ring ] ring_or_no_ring
  | No_ring_pinned : [> `No_ring_pinned ] ring_or_no_ring

let no_ring_to_reference_count : [< `No_ring | `No_ring_pinned ] ring_or_no_ring -> _
  = function
  | No_ring -> 0
  | No_ring_pinned -> 1
;;

type ring = [ `Ring ] ring_or_no_ring

module Config = struct
  type t =
    { attach : bool or_null
    ; sq_entries : int or_null
    ; cq_entries : int or_null
    ; sqpoll : (idle_ms:int * cpu_aff:int or_null) option
    }

  let default = { attach = Null; sq_entries = Null; cq_entries = Null; sqpoll = None }
end

type 'k t_inner =
  { config : Config.t
  ; rings : [ `Ring | `No_ring | `No_ring_pinned ] ring_or_no_ring Atomic.Array.t
  ; awaiters : Awaiters.t
  ; mutable mutex : 'k Sync.Mutex.Loc.State.t [@atomic]
  (** The [mutex] is used to protect certain operations such as creation of rings and
      pinning/unpinning as those are more difficult to make entirely lock-free. *)
  ; mutable pins : int [@atomic]
  }

type t = T : 'k t_inner @@ global -> t [@@unboxed]

let[@inline never] raise_error result ~name =
  raise (Caml_unix.Unix_error (Uring.Result.error_exn result, name, ""))
;;

let consume_available_completions sync awaiters (Ring ring_r : ring) =
  let es =
    let%with.tilde.stack cq =
      Sync.Mutex.Loc.(with_data [@mode local])
        sync
        [%atomic.loc ring_r.cq_mutex]
        ring_r.cq
    in
    exclave_ Uring.Cq.drain cq
  in
  let[@inline] rec loop : _ Uring.Cq.list @ local -> _ = function
    | [] -> ()
    | entry :: es ->
      let tag = Uring.Cq.Entry.tag entry in
      let result = Uring.Cq.Entry.result entry in
      let id = Id.of_tag tag in
      (match Awaiters.find awaiters id with
       | Null -> failwith "bug: awaiter not found"
       | This awaiter ->
         (match
            if Id.is_cancel_tag tag
            then Awaiter.cancelled awaiter result
            else Awaiter.completed awaiter result
          with
          | Died -> Awaiters.remove awaiters id
          | Left_alive -> ());
         loop es)
  in
  loop es [@nontail]
;;

let release_eventfd (Ring ring_r : ring) =
  if 1
     = State_of_a_ring.(eventfd_rc (fetch_and_decr_eventfd_rc [%atomic.loc ring_r.state]))
  then Caml_unix.close (Eventfd.to_file_descr ring_r.cq_consumer_eventfd)
;;

let[@inline never] try_destroy_ring t_r i (Ring ring_r as ring : ring) before =
  let after = before |> State_of_a_ring.and_kill in
  match
    Atomic.Loc.compare_and_set
      [%atomic.loc ring_r.state]
      ~if_phys_equal_to:before
      ~replace_with:after
  with
  | Compare_failed ->
    ( (* Either another thread acquired or destroyed the ring and we are done. *) )
  | Set_here ->
    let _ : Atomic.Compare_failed_or_set_here.t =
      (* At this point we have successfully death marked the [ring]. We then attempt to
         change the ring array to no longer point to the [ring]. If the attempt fails, it
         means that some other thread has either created a new ring or the ring has been
         pinned, i.e. [No_ring_pinned], and there is nothing for us to do except to
         continue to kill the ring. *)
      Atomic.Array.compare_and_set
        t_r.rings
        i
        ~if_phys_equal_to:ring
        ~replace_with:No_ring
    in
    (* Finally we need to ensure the consumer thread is woken up to kill the ring.

       This should not block (i.e. we are not going to reach the [UINT64_MAX - 1] limit). *)
    Eventfd.write ring_r.cq_consumer_eventfd 1L;
    release_eventfd ring
;;

let[@inline] release_ring t_r i (Ring ring_r as ring : ring) =
  let state = State_of_a_ring.fetch_and_decr_ring_rc [%atomic.loc ring_r.state] in
  assert%debug (State_of_a_ring.is_live state);
  if State_of_a_ring.ring_rc state = 1
  then try_destroy_ring t_r i ring (state |> State_of_a_ring.and_decr_ring_rc)
;;

let[@inline never] release_ring_and_reraise t_r i ring exn =
  (* We don't want this inlined as this is not on the hot path and we want to minimize
     code size on the hot path. *)
  let bt = Backtrace.Exn.most_recent () in
  release_ring t_r i ring;
  Exn.raise_with_original_backtrace exn bt
;;

(** The explicit type signature ensures [t_inner.mutex] is already held. *)
let rec pin_ring : 'k Capsule.Access.t -> 'k t_inner @ local -> _ -> _ =
  fun access t_r i ->
  match Atomic.Array.unsafe_get t_r.rings i with
  | No_ring ->
    (match
       Atomic.Array.unsafe_compare_and_set
         t_r.rings
         i
         ~if_phys_equal_to:No_ring
         ~replace_with:No_ring_pinned
     with
     | Set_here -> ()
     | Compare_failed -> pin_ring access t_r i)
  | No_ring_pinned ->
    (* This should never happen, because individual ring/ring slots are pinned only once,
       but this is not statically ruled out. *)
    failwith "Bug: already pinned"
  | Ring ring_r as ring ->
    let state = State_of_a_ring.fetch_and_incr_ring_rc [%atomic.loc ring_r.state] in
    if not (State_of_a_ring.is_live state)
    then (
      match
        Atomic.Array.unsafe_compare_and_set
          t_r.rings
          i
          ~if_phys_equal_to:ring
          ~replace_with:No_ring_pinned
      with
      | Set_here -> ()
      | Compare_failed -> pin_ring access t_r i)
;;

(** The explicit type signature ensures [t_inner.mutex] is already held. *)
let rec unpin_ring : 'k Capsule.Access.t -> 'k t_inner @ local -> _ -> _ =
  fun access t_r i ->
  match Atomic.Array.unsafe_get t_r.rings i with
  | No_ring ->
    (* This should never happen as [unpin] is only called after [pin], but this is not
       statically ruled out. *)
    failwith "Bug: not pinned"
  | No_ring_pinned ->
    (match
       Atomic.Array.unsafe_compare_and_set
         t_r.rings
         i
         ~if_phys_equal_to:No_ring_pinned
         ~replace_with:No_ring
     with
     | Set_here -> ()
     | Compare_failed -> unpin_ring access t_r i)
  | Ring _ as ring -> release_ring t_r i ring
;;

let pin_locked access t_r =
  match Atomic.Loc.fetch_and_add [%atomic.loc t_r.pins] 1 with
  | 0 ->
    (* We only pin the individual rings/ring slots once on the first time [pinned] is
       called so that nested [pinned] calls do not pin individual rings multiple times. *)
    for i = 0 to Atomic.Array.length t_r.rings - 1 do
      pin_ring access t_r i
    done
  | _ -> ()
;;

let pin sync (T t_r) =
  let%with.tilde.stack #(_, access) =
    Sync.Mutex.Loc.with_access sync [%atomic.loc t_r.mutex]
  in
  pin_locked access t_r
;;

let unpin_locked access t_r =
  match Atomic.Loc.fetch_and_add [%atomic.loc t_r.pins] (-1) with
  | 1 ->
    (* We only unpin the individual rings/ring slots once. *)
    for i = Atomic.Array.length t_r.rings - 1 downto 0 do
      unpin_ring access t_r i
    done
  | _ -> ()
;;

let pinned_locked access t ~(f : _ -> _ @ unique) =
  pin_locked access t;
  match f #() with
  | result ->
    unpin_locked access t;
    result
  | exception exn ->
    let bt = Backtrace.Exn.most_recent () in
    unpin_locked access t;
    (match Exn.raise_with_original_backtrace exn bt with
     | (_ : Nothing.t) -> .)
;;

let unpin sync (T t_inner) =
  let%with.tilde.stack #(_sync, access) =
    Sync.Mutex.Loc.with_access sync [%atomic.loc t_inner.mutex]
  in
  unpin_locked access t_inner
;;

let pinned await t ~f =
  let sync = Await.sync await in
  pin sync t;
  match f #() with
  | result ->
    unpin sync t;
    result
  | exception exn ->
    let bt = Backtrace.Exn.most_recent () in
    unpin sync t;
    Exn.raise_with_original_backtrace exn bt
;;

(** [kill_ring ring] acquires both ends of the ring and exits it. This must only be called
    either after the ring has been marked for death (and will no longer be used) or when
    there are no other references to the ring e.g. when something goes wrong after the
    ring is created, but before the ring has been published to other threads. *)
let kill_ring sync (Ring ring_r : ring) =
  let sq =
    Sync.Mutex.Loc.acquire_and_poison_as_owned
      sync
      [%atomic.loc ring_r.sq_mutex]
      ring_r.sq
  in
  let cq =
    Sync.Mutex.Loc.acquire_and_poison_as_owned
      sync
      [%atomic.loc ring_r.cq_mutex]
      ring_r.cq
  in
  Uring.exit #(~sq, ~cq)
;;

let attach_eventfd sync (Ring ring_r : ring) eventfd =
  let%with.tilde.stack cq =
    Sync.Mutex.Loc.with_scoped sync [%atomic.loc ring_r.cq_mutex] ring_r.cq
  in
  let result = Uring.Cq.register_eventfd cq eventfd in
  if Uring.Result.is_error result then raise_error result ~name:"register_eventfd"
;;

let submit_cancellations sync (Ring r : ring) =
  match Atomic.Loc.get [%atomic.loc r.cancellations] with
  | [] -> ()
  | _ :: _ ->
    let cancellations = Atomic.Loc.exchange [%atomic.loc r.cancellations] [] in
    List.iter cancellations ~f:(stack_ fun id ->
      let%with.tilde.stack sq =
        Sync.Mutex.Loc.with_scoped sync [%atomic.loc r.sq_mutex] r.sq
      in
      (* It would be fine to [ensure_can_push] just once, but it is also unlikely that we
         would be frequently pushing many cancellations here anyway. *)
      Uring.Sq.ensure_can_push sq ~n:1;
      Uring.Sq.push_cancel
        sq
        ~tag:(Id.cancel_tag id)
        Uring.Sqe_flags.empty
        ~tag_to_cancel:(Id.to_tag id)
        Uring.Cancel_flags.(all);
      Uring.Sq.submit sq [@nontail])
    [@nontail]
;;

(* Drain the ring of any remaining cancellations and other requests. *)
let drain sync awaiters (Ring ring_r as ring : ring) =
  submit_cancellations sync ring;
  let trigger = Trigger.create () in
  let awaiter = Awaiter.make ~completions:1 ~cancellable:false (Trigger.source trigger) in
  let id = Awaiters.add awaiters awaiter in
  let () =
    let%with.tilde.stack sq =
      Sync.Mutex.Loc.with_scoped sync [%atomic.loc ring_r.sq_mutex] ring_r.sq
    in
    Uring.Sq.ensure_can_push sq ~n:1;
    Uring.Sq.push_nop sq ~tag:(Id.to_tag id) Uring.Sqe_flags.io_drain;
    Uring.Sq.submit sq
  in
  while not (Trigger.is_signalled trigger) do
    (match Eventfd.read ring_r.cq_consumer_eventfd with
     | (_ : int64) -> ()
     | exception Caml_unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> ());
    consume_available_completions sync awaiters ring
  done
;;

let consumer_thread_main awaiters (Ring ring_r as ring : ring) =
  let sync = Sync_blocking.sync in
  (try
     while State_of_a_ring.is_live (Atomic.Loc.get [%atomic.loc ring_r.state]) do
       (match Eventfd.read ring_r.cq_consumer_eventfd with
        | (_ : int64) -> ()
        | exception Caml_unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> ());
       submit_cancellations sync ring;
       consume_available_completions sync awaiters ring
     done;
     drain sync awaiters ring
   with
   | exn ->
     (* We should not be getting exceptions except in case of bugs. We print the exception
        and then fall through. *)
     print_endline (Exn.to_string exn));
  (* Killing the ring will poison the mutexes, which will kill any thread attempting to
     access the ring afterwards in case we are here due to an unhandled exception. *)
  kill_ring sync ring;
  release_eventfd ring
;;

let on_failure cleanup ~f =
  try f #() with
  | exn ->
    let bt = Backtrace.Exn.most_recent () in
    cleanup #();
    Exn.raise_with_original_backtrace exn bt
;;

let rec acquire_ring_slow_and_help await (T t_r as t) i dead_ring =
  (* We are here as we found a death marked ring in the set. Another thread will kill the
     ring and we must not use it. To make progress, i.e. to avoid waiting for the killer
     thread, we "help" it by updating the ring slot by ourselves. *)
  let _ : Atomic.Compare_failed_or_set_here.t =
    Atomic.Array.compare_and_set
      t_r.rings
      i
      ~if_phys_equal_to:dead_ring
      ~replace_with:No_ring
  in
  acquire_ring_slow await t i

and acquire_ring_slow await (T ({ awaiters; _ } as t_r) as t) i =
  let { aliased_many = Ring ring_r as ring } =
    let%with.tilde.stack #(sync, access) =
      Sync.Mutex.Loc.with_access (Await.sync await) [%atomic.loc t_r.mutex]
    in
    match Atomic.Array.get t_r.rings i with
    | Ring _ as ring -> { aliased_many = ring }
    | (No_ring | No_ring_pinned) as no_ring ->
      let cq_consumer_eventfd =
        (Or_error.ok_exn Eventfd.create) ~flags:Eventfd.Flags.cloexec Int32.zero
      in
      let%with.tilde.stack #() =
        on_failure (fun #() ->
          Caml_unix.close (Eventfd.to_file_descr cq_consumer_eventfd))
      in
      let%tydi { attach; sq_entries; cq_entries; sqpoll } = t_r.config in
      let #(~sq, ~cq) =
        (let%with.tilde.stack attach
           ~(f : (_ Option.t[@kind value_or_null & void]) @ local -> _ @ unique)
           =
           if Or_null.value_or_thunk attach ~default:(fun () -> true)
           then (
             (* The config asks us to attach the rings, i.e. to use
                [IORING_SETUP_ATTACH_WQ], which means that the rings will share the
                [io_uring] asynchronous worker thread backend.

                So, we need to check whether some rings already exist and, if so, attach
                the new ring with an existing ring.

                We have already locked the mutex of the ring set, but existing rings could
                still be destroyed concurrently. So, before trying to find an existing
                ring, we pin the set, which prevents existing rings, including the ring we
                want to attach with, from being destroyed. *)
             let%with.tilde.stack #() = pinned_locked access t_r in
             let rec find i =
               if i < Atomic.Array.length t_r.rings
               then (
                 match Atomic.Array.unsafe_get t_r.rings i with
                 | No_ring | No_ring_pinned -> find (i + 1)
                 | Ring ring_r ->
                   if State_of_a_ring.is_live ring_r.state
                   then (
                     let%with.tilde.stack sq =
                       Sync.Mutex.Loc.with_scoped
                         sync
                         [%atomic.loc ring_r.sq_mutex]
                         ring_r.sq
                     in
                     f (Some sq) [@nontail])
                   else find (i + 1))
               else f None
             in
             find 0 [@nontail])
           else f None
         in
         (Box.box [@kind value_or_null & value_or_null] [@mode unique])
           ((Uring.create [@alert "-experimental"])
              ~attach
              ~sq_entries
              ~cq_entries
              ~sqpoll
              ()))
        |> (Box.unbox [@kind value_or_null & value_or_null] [@mode unique])
      in
      let (P #(sq_mutex, sq)) = Sync.Mutex.Loc.of_owned sq in
      let (P #(cq_mutex, cq)) = Sync.Mutex.Loc.of_owned cq in
      let rec ring =
        Ring
          { state =
              (let ring_rc = no_ring_to_reference_count no_ring in
               State_of_a_ring.initial ~ring_rc)
          ; sq_mutex
          ; sq
          ; cq_mutex
          ; cq
          ; cq_consumer_eventfd
          ; cancellations = []
          ; terminate_or_cancel
          }
      and terminate_or_cancel id =
        let (Ring ring_r : ring) = ring in
        let state = State_of_a_ring.fetch_and_incr_ring_rc [%atomic.loc ring_r.state] in
        if State_of_a_ring.is_live state
        then (
          (* We are in a [Trigger] callback, which means we must be lock-free. *)
          (match
             let%with.tilde.stack password =
               Sync.Mutex.Loc.try_with_password [%atomic.loc ring_r.sq_mutex]
             in
             let sq = Capsule.Scoped.P #{ data = ring_r.sq; password } in
             Uring.Sq.ensure_can_push sq ~n:1;
             Uring.Sq.push_cancel
               sq
               ~tag:(Id.cancel_tag id)
               Uring.Sqe_flags.empty
               ~tag_to_cancel:(Id.to_tag id)
               Uring.Cancel_flags.(all + userdata);
             Uring.Sq.submit sq
           with
           | Acquired () -> ( (* We managed to submit cancellation immediately. *) )
           | Would_block ->
             (* We could not obtain the submission queue, so we must use a non-blocking
                approach to cancellation. *)
             let rec loop backoff =
               let before = Atomic.Loc.get [%atomic.loc ring_r.cancellations] in
               let after = id :: before in
               match
                 Atomic.Loc.compare_and_set
                   [%atomic.loc ring_r.cancellations]
                   ~if_phys_equal_to:before
                   ~replace_with:after
               with
               | Compare_failed -> loop (Backoff.once backoff)
               | Set_here ->
                 (* We have successfully stored the cancellation and the consumer thread
                    can see it. We then write to the eventfd to ensure that the consumer
                    thread will wakeup (if sleeping) and see the cancellation. *)
                 Eventfd.write ring_r.cq_consumer_eventfd 1L
             in
             loop Backoff.default);
          release_ring t_r i ring)
        else (
          match Awaiters.find awaiters id with
          | Null -> assert%debug false
          | This awaiter ->
            (match Awaiter.no_longer_cancellable awaiter with
             | Died -> Awaiters.remove t_r.awaiters id
             | Left_alive -> assert%debug false))
      in
      let%with.tilde.stack #() = on_failure (fun #() -> kill_ring sync ring) in
      attach_eventfd sync ring cq_consumer_eventfd;
      (match
         Multicore.spawn_on
           ~domain:(Multicore.current_domain ())
           (fun { aliased_many = ring } -> consumer_thread_main awaiters ring)
           { aliased_many = ring }
       with
       | Spawned ->
         Atomic.Array.set
           t_r.rings
           i
           (match ring with
            | Ring _ as ring -> ring);
         { aliased_many = ring }
       | Failed (_, exn, bt) -> Exn.raise_with_original_backtrace exn bt)
  in
  (* We optimistically increment the reference count and check whether the ring is still
     alive after the increment. The expectation is that the optimistic increment is almost
     always sufficient as the rings are likely pinned. The slow path requires us to create
     a new ring and that is vastly more expensive anyway. *)
  let state = State_of_a_ring.fetch_and_incr_ring_rc [%atomic.loc ring_r.state] in
  if State_of_a_ring.is_live state
  then ring
  else acquire_ring_slow_and_help await t i ring
;;

let[@inline] acquire_ring await (T t_r as t) i =
  match Atomic.Array.unsafe_get t_r.rings i with
  | No_ring | No_ring_pinned -> acquire_ring_slow await t i
  | Ring ring_r as ring ->
    let state = State_of_a_ring.fetch_and_incr_ring_rc [%atomic.loc ring_r.state] in
    if State_of_a_ring.is_live state
    then ring
    else acquire_ring_slow_and_help await t i ring
;;

let with_ring await (T t_r as t) ~f =
  let i = Domain.self_index () in
  assert%debug (0 <= i && i < Atomic.Array.length t_r.rings);
  let ring = acquire_ring await t i in
  match f ring with
  | value ->
    release_ring t_r i ring;
    value
  | exception exn -> release_ring_and_reraise t_r i ring exn
;;

let call_sq_or_cancel await cancel (T t_r as t) ~n ~f =
  if n <= 0 then invalid_arg "number of requests must be strictly positive";
  let%with.tilde.stack (Ring ({ terminate_or_cancel; _ } as ring_r) as ring) =
    with_ring await t
  in
  if n > Uring.Sq.entries ring_r.sq
  then invalid_arg "attempting to push too many requests";
  let await_on = Trigger.create () in
  let awaiter = Awaiter.make ~completions:n ~cancellable:true (Trigger.source await_on) in
  let id = Awaiters.add t_r.awaiters awaiter in
  (* We assume that the code below does not normally raise exceptions. If it does raise,
     it should indicate a bug and will likely leave things in an invalid state.

     The basic issue is that we need a number of things to match. We expect [f] to push
     (at most) [n] requests and then expect to receive exactly [n] completions. This is so
     that we know when the (set of) operation(s) [f] submitted has fully completed and can
     return.

     We also allow a cancellation (to be submitted) and we also account for this.

     We also must not reuse the [id/tag] before all the requests using it have been
     completed.

     So, removing the [awaiter] is not sufficient to clean up things properly in case of
     an unexpected exception. *)
  let () =
    let%with.tilde.stack sq =
      Sync.Mutex.Loc.with_scoped
        (Await.sync await)
        [%atomic.loc ring_r.sq_mutex]
        ring_r.sq
    in
    Uring.Sq.ensure_can_push sq ~n;
    f #(sq, Id.to_tag id);
    Uring.Sq.submit sq
  in
  consume_available_completions (Await.sync await) t_r.awaiters ring;
  (match
     Await.await_with_terminate_or_cancel await cancel await_on ~terminate_or_cancel id
   with
   | Dropped ->
     (* [terminate_or_cancel] won't be called and we must remove the awaiter. *)
     (match Awaiter.no_longer_cancellable awaiter with
      | Died -> Awaiters.remove t_r.awaiters id
      | Left_alive -> assert%debug false)
   | Already_signalled ->
     ( (* [terminate_or_cancel] was/will be called and will ensure removal of the awaiter. *) ));
  Awaiter.get awaiter
;;

let call_sq await t ~n ~f = call_sq_or_cancel await Cancellation.never t ~n ~f

let[@inline never] raise_terminated_or_canceled_or_error await result ~name =
  if Uring.Result.is_canceled result
  then
    if Terminator.is_terminated (Await.terminator await)
    then raise Await.Terminated
    else raise_notrace Or_canceled.Exn.Canceled
  else raise_error result ~name
;;

let[@inline] on_error_raise_terminated_or_canceled_or_error await result ~name =
  if Uring.Result.is_error result
  then raise_terminated_or_canceled_or_error await result ~name
;;

(** Call with SQ expecting [0] (or a negated error code) as the result. *)
let call_sq_0_or_cancel_exn await cancel t ~n ~name ~f =
  let result = call_sq_or_cancel await cancel t ~n ~f in
  on_error_raise_terminated_or_canceled_or_error await result ~name
;;

let call_sq_0_exn await t ~n ~name ~f =
  call_sq_0_or_cancel_exn await Cancellation.never t ~n ~name ~f [@nontail]
;;

(** Call with SQ expecting non-negative value (or a negated error code) as the result. *)
let call_sq_non_neg_or_cancel_exn await cancel t ~n ~name ~f =
  let result = call_sq_or_cancel await cancel t ~n ~f in
  if Uring.Result.is_ok result
  then Uring.Result.ok_exn result
  else raise_terminated_or_canceled_or_error await result ~name
;;

(** Call with SQ expecting (non-negative) file descriptor (or a negated error code) as the
    result. *)
let call_sq_fd_or_cancel_exn await cancel t ~n ~name ~f =
  let result = call_sq_non_neg_or_cancel_exn await cancel t ~n ~name ~f in
  Core_unix.File_descr.of_int result
;;

let call_sq_fd_exn await t ~n ~name ~f =
  call_sq_fd_or_cancel_exn await Cancellation.never t ~n ~name ~f
;;

let create config =
  let rings =
    let len = Stdlib.Domain.recommended_domain_count () in
    Atomic.Array.create ~len No_ring
  in
  let awaiters = Awaiters.create () in
  let (P key) = Capsule.Prim.create () in
  let mutex = Sync.Mutex.Loc.make key in
  T { config; rings; awaiters; mutex; pins = 0 }
;;

module For_testing = struct
  let appears_idle (T t_r) = Awaiters.For_testing.is_empty t_r.awaiters
end
