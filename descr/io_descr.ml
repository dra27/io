open Base
open Portable

module Bits = struct
  (* The same bits and bit fields are used for [Roles], [Actions], and the [State]. This
     way many conversions between them can be avoided (or those conversions are the
     identity function). *)

  include Int63

  (* The state looks like this:

     {v
       Bit: [    0     |      1 - 30    |    31 - 60     ]
       Use: [ shutdown | readable count | writable count ]
     v}

     In other words, there is a single configuration bit [shutdown_bit] and two reference
     counts for the [readable] and [writable] sides of a descriptor. *)

  module Constants = struct
    let shutdown = of_int64_trunc 0x0000_0000_0000_0001L
    let readable = of_int64_trunc 0x0000_0000_0000_0002L
    let writable = of_int64_trunc 0x0000_0000_8000_0000L
    let to_mask ones = O.((ones lsl 30) - ones)
  end

  let of_bool b = of_int (Bool.to_int b)
end

module Roles = struct
  let readable = Bits.Constants.readable
  let writable = Bits.Constants.writable

  include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = true
      let known = [ readable, "readable"; writable, "writable" ]
    end)

  let empty = `Should_not_be_used
end

module Actions = struct
  let shutdown_rd = Bits.Constants.readable
  let shutdown_wr = Bits.Constants.writable
  let and_close = Bits.Constants.shutdown

  include Flags.Make (struct
      let allow_intersecting = false
      let should_print_error = true
      let remove_zero_flags = true

      let known =
        [ shutdown_rd, "shutdown_rd"; shutdown_wr, "shutdown_wr"; and_close, "and_close" ]
      ;;
    end)

  let empty = `Should_not_be_used
end

module State : sig @@ portable
  type t : immediate64

  val init : Roles.t -> shutdown:bool -> t
  val should_close : after:t -> bool
  val and_incr : Roles.t -> t -> t
  val and_decr : Roles.t -> t -> #(Actions.t * t)
  val and_decr_after : Actions.t -> t -> t
end = struct
  type t = Bits.t

  open Bits.O
  open Bits.Constants

  let[@inline] has_0 roles_or_shutdowns t =
    let rd () =
      Roles.(do_intersect readable) roles_or_shutdowns && t land to_mask readable = zero
    in
    let wr () =
      Roles.(do_intersect writable) roles_or_shutdowns && t land to_mask writable = zero
    in
    rd () || wr ()
  ;;

  let[@inline] has_max roles_or_shutdowns t =
    let rd () =
      Roles.(do_intersect readable) roles_or_shutdowns
      && t land to_mask readable = to_mask readable
    in
    let wr () =
      Roles.(do_intersect writable) roles_or_shutdowns
      && t land to_mask writable = to_mask writable
    in
    rd () || wr ()
  ;;

  let[@inline] init roles ~shutdown = Bits.of_bool shutdown lor roles

  let[@inline] and_incr roles before =
    assert%debug (not (has_0 roles before));
    assert%debug (not (has_max roles before));
    before + roles
  ;;

  let should_close ~after = after land to_mask (readable lor writable) = zero

  let[@inline never] and_decr roles before =
    assert%debug (not (has_0 roles before));
    (* The logic below is intentionally written to be branchless. *)
    let roles_mask = to_mask roles in
    let shutdown_mask =
      (* As [shutdown = 1] this will result in either all [1]s or all [0]s. *)
      -(before land shutdown)
    in
    let after =
      let after_candidate = before - roles in
      let after_masked = after_candidate land shutdown_mask in
      Bool.select
        (Bool.equal
           (after_masked land roles_mask = zero)
           (should_close ~after:after_masked))
        after_candidate
        before
    in
    let actions =
      let before_masked = before land roles_mask land shutdown_mask in
      let shutdown_rd_bit =
        Bits.of_bool (before_masked land to_mask readable = readable)
        * Actions.shutdown_rd
      in
      let shutdown_wr_bit =
        Bits.of_bool (before_masked land to_mask writable = writable)
        * Actions.shutdown_wr
      in
      let and_close_bit = Bits.of_bool (should_close ~after) * Actions.and_close in
      Actions.(shutdown_rd_bit + shutdown_wr_bit + and_close_bit)
    in
    #(actions, after)
  ;;

  let[@inline] and_decr_after actions before =
    assert%debug (not (Actions.(do_intersect and_close) actions));
    assert%debug (not (has_0 actions before));
    before - actions
  ;;
end

type raw_fd : immediate

type inner =
  { mutable rc : State.t [@atomic]
  ; fd : raw_fd
  }

type t = inner Modes.Global.t

let incr roles t =
  let rec loop backoff =
    let before = Atomic.Loc.get [%atomic.loc t.global.rc] in
    let after = before |> State.and_incr roles in
    match
      Atomic.Loc.compare_and_set
        [%atomic.loc t.global.rc]
        ~if_phys_equal_to:before
        ~replace_with:after
    with
    | Set_here -> ()
    | Compare_failed -> loop (Backoff.once backoff)
  in
  loop Backoff.default [@nontail]
;;

let decr roles t =
  let[@inline] rec loop backoff =
    let before = Atomic.Loc.get [%atomic.loc t.global.rc] in
    let #(actions, after) = before |> State.and_decr roles in
    if phys_equal before after
       || phys_equal
            before
            (Atomic.Loc.compare_exchange
               [%atomic.loc t.global.rc]
               ~if_phys_equal_to:before
               ~replace_with:after)
    then actions
    else loop (Backoff.once backoff)
  in
  loop Backoff.default [@nontail]
;;

let should_close_after actions t =
  let rec loop backoff =
    let before = Atomic.Loc.get [%atomic.loc t.global.rc] in
    let after = before |> State.and_decr_after actions in
    if phys_equal before after
       || phys_equal
            before
            (Atomic.Loc.compare_exchange
               [%atomic.loc t.global.rc]
               ~if_phys_equal_to:before
               ~replace_with:after)
    then State.should_close ~after
    else loop (Backoff.once backoff)
  in
  loop Backoff.default [@nontail]
;;

let[@inline] create roles ~shutdown fd =
  let rc = State.init roles ~shutdown in
  { global = { fd; rc } }
;;

let[@inline] unsafe_get t = t.global.fd
let[@inline] globalize (t : t) = t
