(* OS.FileSys.chDir "../exercise/chp6" *)

(* SML/NJのlazy機能の準備 *)
(*
 * SML/NJのlazyを有効にする
 * Control.lazysml以外には、`sml -Cparser.lazy-keyword=true`で起動してもOK
 *)
val () = Control.lazysml := true
open Lazy                       (* $コンストラクタをトップレベルに呼び出し *)
fun force ($ x) = x             (* forceがないので実装 *)
infixr ++;

use "../../original/chp4.sml";

signature QUEUE =
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a         (* raises Empty if queue is empty *)
  val tail    : 'a Queue -> 'a Queue   (* raises Empty if queue is empty *)
end

local
  open Stream
in
structure RealTimeQueue : QUEUE =
struct
  type 'a Queue = 'a Stream * 'a list * 'a Stream

  val empty = ($Nil, [], $Nil)
  fun isEmpty ($Nil, _, _) = true
    | isEmpty _ = false

  fun rotate ($Nil, [y], a) = $(Cons (y, a))
    | rotate ($Nil, _ :: _, a) =
      raise Fail "RealTimeQueue.rotate: not supported pattern"
    | rotate ($(Cons (x, xs)), y :: ys, a) =
        $(Cons (x, rotate (xs, ys, $(Cons (y, a)))))

  fun exec (f, r, $(Cons (x, s))) = (f, r, s)
    | exec (f, r, $Nil) = let val f' = rotate (f, r, $Nil) in (f', [], f') end

  fun snoc ((f, r, s), x) = exec (f, x :: r, s)

  fun head ($Nil, r, s) = raise Empty
    | head ($(Cons (x, f)), r, s) = x
  fun tail ($Nil, r, s) = raise Empty
    | tail ($(Cons (x, f)), r, s) = exec (f, r, s)
end
end

(* 'a RealTimeQueue.Queue -> int *)
local
  open Stream
in
fun sizeOfStream ($Nil) = 0
  | sizeOfStream ($(Cons (_, s))) = 1 + sizeOfStream s
end

fun sizeQueue1 (f, r, _) = sizeOfStream f + length r
(*
 * |s| = |f| - |r| + 1、より
 * |f| = |s| + |r| - 1、なので、
 * (キューのサイズ) = |f| + |r|
 *                  = |s| + 2|r| - 1
 *)
fun sizeQueue2 (_, r, s) = sizeOfStream s + 2 * length r - 1

(*
 * それぞれの実行時間の差を考える。
 *
 * 違いは、キューのサイズを計算するために、
 * Streamのサイズにfを使うかsを使うかである。
 *
 * ここで、sはfの接尾辞であるため、|s|<=|f|が保証される。
 * つまり、sizeOfStreamを呼び出したときの時間は、fよりもsのほうが同等か短い。
 *
 * よって、f=sのときはsizeQueue1のほうが乗算1回加算1回分早いが、
 * それ以外の場合はsizeQueue2のほうが早くなる。
 *)
