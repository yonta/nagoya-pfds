signature QUEUE =
sig
  type 'a Queue
  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool
  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a
  val tail    : 'a Queue -> 'a Queue
end

structure HoodMelvilleQueue : QUEUE =
struct
  datatype 'a RotationState =
           Idle
         | Reversing of int * 'a list * 'a list * 'a list * 'a list * int
         | Appending of int * 'a list * 'a list * int
         | Done of 'a list * int

  type 'a Queue = int option * 'a list * 'a RotationState * 'a list

  fun exec (Reversing (ok, x :: f, f', y :: r, r', lenr)) =
      Reversing (ok+1, f, x :: f', r, y :: r', lenr+1)
    | exec (Reversing (ok, [], f', [y], r', lenr)) =
      Appending (ok, f', y :: r', lenr+1)
    | exec (Reversing (_, [], _, _, _, _)) = raise Fail "Bug:exec: f=[]"
    | exec (Appending (0, f', r', lenr)) = Done (r', lenr)
    | exec (Appending (ok, x :: f', r', lenr)) =
      Appending (ok-1, f', x :: r', lenr)
    | exec state = state        (* Idle and Done *)


  fun invalidate (Reversing (ok, f, f', r, r', lenr)) =
      Reversing (ok-1, f, f', r, r', lenr)
    | invalidate (Appending (0, f', x :: r', lenr)) = Done (r', lenr-1)
    | invalidate (Appending (ok, f', r', lenr)) =
      Appending (ok-1, f', r', lenr)
    | invalidate state = state

  fun exec2 (diffopt, f, state, r) =
      case exec (exec state) of
          Done (f, diff) => (SOME diff, f, Idle, r)
        | newstate => (diffopt, f, newstate, r)

  fun check (q as (NONE, _, _, _)) = exec2 q
    | check (q as (SOME diff, f, state, r)) =
      if diff >= 0 then exec2 q
      else let val newstate = Reversing (0, f, [], r, [], 0)
           in exec2 (NONE, f, newstate, []) end

  val empty = (SOME 0, [], Idle, [])
  fun isEmpty (lenf, f, state, r) = null f andalso null r

  fun snoc ((NONE, f, state, r), x) = check (NONE, f, state, x::r)
    | snoc ((SOME diff, f, state, r), x) = check (SOME (diff-1), f, state, x::r)
  fun head (_, [], _, _) = raise Empty
    | head (_, x :: f, _, _) = x
  fun tail (_, [], _, _) = raise Empty
    | tail (NONE, _::f, state, r) = check (NONE, f, invalidate state, r)
    | tail (SOME diff, _::f, state, r) =
      check (SOME (diff-1), f, invalidate state, r)
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
