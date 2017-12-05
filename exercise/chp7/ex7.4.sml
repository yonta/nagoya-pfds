(* OS.FileSys.chDir "../exercise/chp7" *)

(* SML/NJのlazy機能の準備 *)
(*
 * SML/NJのlazyを有効にする
 * Control.lazysml以外には、`sml -Cparser.lazy-keyword=true`で起動してもOK
 *)
val () = Control.lazysml := true
open Lazy                       (* $コンストラクタをトップレベルに呼び出し *)
fun force ($ x) = x             (* forceがないので実装 *)
infixr ++;

use "../../original/chp4.sml";  (* Stream *)

signature ORDERED =
sig
  type T

  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature HEAP =
sig
  structure Elem : ORDERED

  type Heap

  val empty     : Heap
  val isEmpty   : Heap -> bool

  val insert    : Elem.T * Heap -> Heap
  val merge     : Heap * Heap -> Heap

  val findMin   : Heap -> Elem.T   (* raises Empty if heap is empty *)
  val deleteMin : Heap -> Heap     (* raises Empty if heap is empty *)
end

local
  open Stream
in
functor ScheduledBinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of Elem.T * Tree list
  datatype Digit = Zero | One of Tree
  type Schedule = Digit Stream list
  type Heap = Digit Stream * Schedule

  val empty = ($Nil, [])

  fun isEmpty ($Nil, _) = true | isEmpty _ = false

  fun link (t1 as Node (x1, c1), t2 as Node (x2, c2)) =
    if Elem.leq (x1, x2) then Node (x1, t2 :: c1) else Node (x2, t1 :: c2)

  fun insTree (t, $Nil) = $(Cons (One t, $Nil))
    | insTree (t, $(Cons (Zero, ds))) = $(Cons (One t, ds))
    | insTree (t, $(Cons (One t', ds))) =
      $(Cons (Zero, insTree (link (t, t'), ds)))

  fun mrg (ds1, $Nil) = ds1
    | mrg ($Nil, ds2) = ds2
    | mrg ($(Cons (Zero, ds1)), $(Cons (d, ds2))) = $(Cons (d, mrg (ds1, ds2)))
    | mrg ($(Cons (d, ds1)), $(Cons (Zero, ds2))) = $(Cons (d, mrg (ds1, ds2)))
    | mrg ($(Cons (One t1, ds1)), $(Cons (One t2, ds2))) =
      $(Cons (Zero, insTree (link (t1, t2), mrg (ds1, ds2))))

  fun normalize (ds as $Nil) = ds
    | normalize (ds as $(Cons (_, ds'))) = (normalize ds'; ds)

  fun exec [] = []
    | exec (($(Cons (Zero, job))) :: sched) = job :: sched
    | exec (_ :: sched) = sched

  fun insert (x, (ds, sched)) =
    let val ds' = insTree (Node (x, []), ds)
    in (ds', exec (exec (ds' :: sched))) end

  fun merge ((ds1, _), (ds2, _)) =
    let val ds = normalize (mrg (ds1, ds2)) in (ds, []) end

  fun removeMinTree ($Nil) = raise Empty
    | removeMinTree ($(Cons (One t, $Nil))) = (t, $Nil)
    | removeMinTree ($(Cons (Zero, ds))) =
      let val (t', ds') = removeMinTree ds in (t', $(Cons (Zero, ds'))) end
    | removeMinTree ($(Cons (One (t as Node (x, _)), ds))) =
      case removeMinTree ds of
          (t' as Node (x', _), ds') =>
          if Elem.leq (x, x') then (t, $(Cons (Zero, ds)))
          else (t', $(Cons (One t, ds')))

  fun findMin (ds, _) = let val (Node (x, _), _) = removeMinTree ds in x end

  fun listToStream nil = $Nil
    | listToStream (h::t) = $(Cons (h, listToStream t))

  (* ex7.4
   * val mrgWithList (ts, ds)
   *     : Tree list * Digit Stream -> Digiti Stream
   * tsとdsをマージする
   *
   * 元のバージョンと比べると、Oneコンストラクタの呼び出し回数が少ないため、
   * データ構造の生成回数が少ない。
   *)
  (*                                                       ts + ds *)
  fun mrgWithList (nil, ds) = ds                         (* 0 + _ *)
    | mrgWithList (t::ts, $(Cons (Zero, ds))) =          (* 1 + 0 *)
      $(Cons (One t, mrgWithList (ts, ds)))
    | mrgWithList (t::ts, $(Cons (One t', ds))) =        (* 1 + 1、繰り上がり *)
      let
        val newT = link (t, t')
        val newDS = mrgWithList (ts, ds)
      in
        $(Cons (Zero, insTree (newT, newDS)))
      end
    | mrgWithList (ts, $Nil) = listToStream (map One ts) (* 1 + 0 *)

  fun deleteMin (ds, _) =
    let
      val (Node (x, c), ds') = removeMinTree ds
      val ds'' = mrgWithList (rev c, ds')
    in
      (normalize ds'', [])
    end

end (* struct *)
end (* local *)

(* test *)
local
  structure MyInt : ORDERED =
  struct
    open Int
    type T = Int.int
    fun eq (x : int, y) = x = y
    fun lt (x, y) = x < y
    fun leq (x, y) = x <= y
  end
  structure SBH = ScheduledBinomialHeap (MyInt)
in
val h0 = SBH.empty
val h1 = SBH.insert (1, h0)
val h2 = SBH.insert (2, h1)
val h3 = SBH.insert (3, h2)
val h4 = SBH.insert (4, h3)
val h5 = SBH.insert (5, h4)
val h6 = SBH.insert (6, h5)
end
