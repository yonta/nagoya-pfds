signature QUEUE =
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a         (* raises Empty if queue is empty *)
  val tail    : 'a Queue -> 'a Queue   (* raises Empty if queue is empty *)
end

structure HoodMelvilleQueue : QUEUE =
struct
  datatype 'a RotationState =
           Idle
         | Reversing of int * 'a list * 'a list * 'a list * 'a list
         | Appending of int * 'a list * 'a list
         | Done of 'a list

  type 'a Queue = int * 'a list * 'a RotationState * int * 'a list

  fun exec (Reversing (ok, x :: f, f', y :: r, r')) =
      Reversing (ok+1, f, x :: f', r, y :: r')
    | exec (Reversing (ok, [], f', [y], r')) = Appending (ok, f', y :: r')
    | exec (Appending (0, f', r')) = Done r'
    | exec (Appending (ok, x :: f', r')) = Appending (ok-1, f', x :: r')
    | exec state = state

  fun invalidate (Reversing (ok, f, f', r, r')) = Reversing (ok-1, f, f', r, r')
    | invalidate (Appending (0, f', x :: r')) = Done r'
    | invalidate (Appending (ok, f', r')) = Appending (ok-1, f', r')
    | invalidate state = state

  (* ex8.2 *)
  fun exec1 (lenf, f, state, lenr, r) =
      case exec state of
          Done newf => (lenf, newf, Idle, lenr, r)
        | newstate => (lenf, f, newstate, lenr, r)

  fun exec3 (lenf, f, state, lenr, r) =
      case exec (exec (exec state)) of
          Done newf => (lenf, newf, Idle, lenr, r)
        | newstate => (lenf, f, newstate, lenr, r)

  fun check (q as (lenf, f, state, lenr, r)) =
      if lenr <= lenf then exec1 q
      else let val newstate = Reversing (0, f, [], r, [])
           in exec3 (lenf+lenr, f, newstate, 0, []) end (* 初回に+2回 *)

  val empty = (0, [], Idle, 0, [])
  fun isEmpty (lenf, f, state, lenr, r) = (lenf = 0)

  fun snoc ((lenf, f, state, lenr, r), x) =
      check (lenf,f,state,lenr+1,x::r)
  fun head (lenf, [], state, lenr, r) = raise Empty
    | head (lenf, x :: f, state, lenr, r) = x
  fun tail (lenf, [], state, lenr, r) = raise Empty
    | tail (lenf, x :: f, state, lenr, r) =
      check (lenf-1, f, invalidate state, lenr, r)
end

(* using *)
local
  open HoodMelvilleQueue
  fun snocN 0 q = q
    | snocN n q = snocN (n-1) (snoc (q, n))
  fun tailN 0 q = q
    | tailN n q = tailN (n-1) (tail q)
in
  val q0 = empty
  val q1 = snoc (q0, 1)
  val q2 = snoc (q1, 2)
  val q3 = snoc (q2, 3)
  val q4 = snoc (q3, 4)
  val q5 = tail q4
  val q6 = tail q5
  val q7 = tail q6
  val q8 = tail q7
  val q100x2 = tailN 100 (snocN 100 q0)
end
