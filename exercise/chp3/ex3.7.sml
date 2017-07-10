(* use "../../original/base.sml"; *)
use "../../original/chp3.sml";

(* OS.FileSys.chDir "../exercise/chp3" *)

(* SML/NJのプリンタ設定を変える *)
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)

functor ExplicitMin (H: HEAP) : HEAP =
struct
  structure Elem = H.Elem

  datatype Heap = E | NE of Elem.T * H.Heap

  val empty = E
  fun isEmpty E = true
    | isEmpty (NE _) = false

  fun insert (elem, E) = NE (elem, H.empty)
    | insert (elem, NE (minE, h)) =
      if H.Elem.lt (elem, minE) then NE (elem, H.insert (minE, h))
      else NE (minE, H.insert (elem, h))

  fun merge (heap, E) = heap
    | merge (E, heap) = heap
    | merge (NE (e1, h1), NE (e2, h2)) =
      if H.Elem.lt (e1, e2) then NE (e1, H.merge (h1, H.insert (e2, h2)))
      else NE (e2, H.merge (H.insert (e1, h1), h2))

  fun findMin E = raise Empty
    | findMin (NE (e, _)) = e

  fun deleteMin E = raise Empty
    | deleteMin (NE (_, h)) =
      let
        val newMin = H.findMin h
        val newH = H.deleteMin h
      in
        NE (newMin, newH)
      end
end

(* print ex 3.7 *)
local
  structure Elem : ORDERED =
  struct
    open Int
    type T = int
    fun eq (x : T, y) = x = y     (* delete warning of polyEqual *)
    fun lt (x, y) = x < y
    fun leq (x, y) = x <= y
  end
  structure MyBH = BinomialHeap (Elem)
  structure MyEM = ExplicitMin(MyBH)
  open MyEM
  val bh = empty;
  val bh1 = insert (1, bh)
  val bh12 = insert (2, bh1)
  val bh123 = insert (3, bh12)
  val bh1234 = insert (4, bh123)
in
val bh12345 = insert (5, bh1234)
val e = findMin bh12345
val bh2345 = deleteMin bh12345
val bh1 = bh1
val bh1_5 = merge (bh2345, bh1)
end
