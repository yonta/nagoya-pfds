(* OS.FileSys.chDir "./exercise/chp5" *)

(* use "../../original/base.sml"; *)
use "../../original/chp5.sml";

(* SML/NJのプリンタ設定を変える
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)
 *)

(*
 *                   f     r
 *                ------------
 *  head * tail <-      |       <- snoc
 *  cons        ->      |       -> last * init
 *                ------------
 *
 * 条件1: fが空かつrが2要素以上なら、rを半分にして新しいfとrを作る
 * 条件2: rが空かつfが2要素以上なら、fを半分にして新しいfとrを作る
 *)
(* ex5.1 (a) *)
structure Deque : DEQUE =
struct
  type 'a Queue = 'a list * 'a list

  val empty = (nil, nil)

  fun isEmpty (nil, nil) = true
    | isEmpty _ = false

  fun share 0 to from = (to, from)
    | share i to (x::from) = share (i-1) (x::to) from
    | share _ _ _ = raise Empty

  fun half nil = (nil, nil)
    | half l =
      let
        val i = length l div 2
      in
        share i nil l
      end

  fun check (nil, r as _::_::_) = half r
    | check (f as _::_::_, nil) =
      let
        val (r, f) = half f
      in
        (f, r)
      end
    | check q = q

  fun cons (x, (f, nil)) = check (x::f, nil)
    | cons (x, (f, r)) = (x::f, r)

  fun head ((x::_, _)) = x
    | head (nil, [x]) = x
    | head _ = raise Empty

  fun tail (_::f, r) = check (f, r)
    | tail (nil, [x]) = empty
    | tail _ = raise Empty

  fun snoc ((nil, r), x) = check (nil, x::r)
    | snoc ((f, r), x) = (f, x::r)

  fun last (_, x::_) = x
    | last ([x], nil) = x
    | last _ = raise Empty

  fun init (f, _::r) = check (f, r)
    | init ([x], nil) = empty
    | init _ = raise Empty
end

local
  open Deque
in
val d0 = empty
val d1 = cons (0, d0)
val d2 = cons (1, d1)
val d3 = cons (2, d2)
val d4 = snoc (d3, 3)
val d5 = init d4
val d6 = init d5
end
