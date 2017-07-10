(* use "../../original/base.sml"; *)
use "../../original/chp3.sml";

(* OS.FileSys.chDir "../exercise/chp3" *)

(* SML/NJのプリンタ設定を変える *)
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)

(* ex 3.6 *)
functor BinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of Elem.T * Tree list
  type Heap = (int * Tree) list

  val empty = []
  fun isEmpty ts = null ts

  (* int * Tree -> int *)
  fun rank (r, _) = r
  (* int * Tree -> Tree *)
  fun tree (_, t) = t

  (* Tree -> Elem.T *)
  fun nodeRoot (Node (x, _)) = x
  (* Tree -> Tree list *)
  fun nodeTree (Node (_, t)) = t

  (* 前提条件：t1のランク = t2のランク *)
  (* Tree * Tree -> Tree *)
  fun linkTree (t1 as Node (x1, c1), t2 as Node (x2, c2)) =
    if Elem.leq (x1, x2) then Node (x1, t2 :: c1)
    else Node (x2, t1 :: c2)

  (* 前提条件：r <= rank *)
  (* int * Tree * Heap -> Heap *)
  fun insTree (r, t, nil) = [(r, t)]
    | insTree (r, t, heap as (rank, tree) :: hs) =
      if r < rank then (r, t) :: heap
      else insTree (r + 1, linkTree (t, tree), hs)

  fun insert (x, heap) = insTree (0, Node (x, []), heap)

  fun merge (h, nil) = h
    | merge (nil, h) = h
    | merge (heap1 as (h1 as (rank1, tree1)) :: hs1,
             heap2 as (h2 as (rank2, tree2)) :: hs2) =
      if rank1 < rank2 then h1 :: merge (hs1, heap2)
      else if rank2 < rank1 then h2 :: merge (heap1, hs2)
      else insTree (rank1 + 1, linkTree (tree1, tree2), merge (hs1, hs2))

  (* Heap -> (int * Tree) * Heap *)
  fun removeMinHeap nil = raise Empty
    | removeMinHeap [h] = (h, nil)
    | removeMinHeap ((h as (rank, tree)) :: hs) =
      let
        val (h' as (rank', tree'), hs') = removeMinHeap hs
      in
        if Elem.leq (nodeRoot tree, nodeRoot tree') then (h, hs)
        else (h', h :: hs')
      end

  fun findMin hs = let val ((_, t), _) = removeMinHeap hs in nodeRoot t end

  (* int * Tree list -> (int * Tree) list *)
  fun zipWithRank (rank, trees) =
    let
      val ranks = List.tabulate (rank, fn r => r)
    in
      ListPair.zip (ranks, trees)
    end

  fun deleteMin heap =
    let
      val ((rank, Node (x, trees)), heap') = removeMinHeap heap
      val trees = rev trees
      val ranks = List.tabulate (rank, fn r => r)
      val newHeap = ListPair.zip (ranks, trees)
    in
      merge (newHeap, heap')
    end
end

(* print ex 3.6 *)
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
  open MyBH
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
