signature LAZY =
sig
  type 'a susp
  val $ : (unit -> 'a) -> 'a susp
  val force : 'a susp -> 'a
end

structure Lazy :> LAZY =
struct
  datatype 'a lazy = Done of 'a | Lazy of unit -> 'a

  type 'a susp = 'a lazy ref

  fun $ cls = ref (Lazy cls)

  fun force (ref (Done x)) = x
    | force (s as ref (Lazy cls)) =
  let
    val x = cls ()
  in
    x before s := Done x
  end
end

signature STREAM =
sig
  datatype 'a streamcell = Nil | Cons of 'a * 'a streamcell Lazy.susp
  type 'a stream = 'a streamcell Lazy.susp
  (* Oooops, limitation of SML Def. *)
  (*
  datatype 'a streamcell = Cons of 'a * 'a stream | Nil
  withtype 'a stream = 'a streamcell susp
   *)

  val null    : 'a stream -> bool
  val ++      : 'a stream * 'a stream -> 'a stream
  val take    : int * 'a stream -> 'a stream
  val drop    : int * 'a stream -> 'a stream
  (* val reverse : 'a stream -> 'a stream *)
end

local
  open Lazy
in
structure Stream : STREAM =
struct
  datatype 'a streamcell = Nil | Cons of 'a * 'a stream
  withtype 'a stream = 'a streamcell susp

  (* streamをopaqueにした場合はnull/hd/tlを提供する *)
  (*
  fun hdc sc = case sc of Cons (a, _) => a | Nil => raise Empty
  fun tlc sc = case sc of Cons (_, s) => force s | Nil => raise Empty
  fun hd s = case force s of Cons (a, _) => a | Nil => raise Empty
  fun tl s = $ (fn () => case force s of Cons (_, s) => s | Nil => raise Empty)
   *)

  fun null s = case force s of Nil => true | Cons _ => false

  infix ++
  fun s1 ++ s2 =
    $ (fn () =>
          case force s1 of
              Cons (x, s) => Cons (x, s ++ s2)
            | Nil => force s2)

  fun take (0, _) = $ (fn () => Nil)
    | take (n, s) =
      $ (fn () =>
            case force s of
                Nil => Nil
              | Cons (x, s') => Cons (x, take (n-1, s')))

  (* int * 'a stream -> 'a stream *)
  fun strictDrop (0, s) = s
    | strictDrop (n, s) =
      case force s of
          Nil => $ (fn () => Nil)
        | Cons (_, s') => strictDrop (n-1, s')

  fun drop (n, s) = $ (fn () => force (strictDrop (n, s)))
end
end  (* local open Lazy *)

signature QUEUE =
sig
  type elem
  type queue

  val empty   : queue
  val isEmpty : queue -> bool

  val snoc    : queue * elem -> queue
  val head    : queue -> elem
  val tail    : queue -> queue
end

(*
 * value restrictionを避けるために、RealTimeQueueモジュールは
 * structureではなくfunctorとしてtypeを受け取る。
 * これにより、Queueの実体の生成時にQueueは一意の型を要素として持つ。
 *)
functor RealTimeQueue (type t) : QUEUE =
struct
  type elem = t

  type queue =
       {
         front : elem Stream.stream,
         rear : elem list,
         accum : elem Stream.stream
       }

  val empty : queue =
      {
        front = Lazy.$ (fn () => Stream.Nil),
        rear = nil,
        accum = Lazy.$ (fn () => Stream.Nil)
      }

  fun isEmpty ({front, ...} : queue) = Stream.null front

  (* fun makeQueue f r a = {front = f, rear = r, accum = a} *)

  (* 'a queue -> 'a stream *)
  fun rotate {front, rear, accum} =
    case (Lazy.force front, rear) of
        (Stream.Nil, [y]) => Lazy.$ (fn () => Stream.Cons (y, accum))
      | (Stream.Cons (x, xs), y :: ys) =>
        Lazy.$ (fn () =>
              Stream.Cons (x, rotate {front = xs, rear = ys,
                                      accum =
                                      Lazy.$ (fn () => Stream.Cons (y, accum))}))
      | _ => raise Fail "RealTimeQueue.rotate: not supported pattern"

  fun exec (q as {front, rear, accum}) =
    case Lazy.force accum of
        Stream.Cons (_, s) => {front = front, rear = rear, accum = s}
      | Stream.Nil => let val front' = rotate q
                      in {front = front', rear = nil, accum = front'} end

  fun snoc ({front, rear, accum}, x) =
    exec {front = front, rear = x :: rear, accum = accum}

  fun head ({front, ...} : queue) =
    case Lazy.force front of
        Stream.Cons (x, _) => x
      | Stream.Nil => raise Empty

  fun tail {front, rear, accum} =
    case Lazy.force front of
        Stream.Cons (x, s) => exec {front = s, rear = rear, accum = accum}
      | Stream.Nil => raise Empty
end
