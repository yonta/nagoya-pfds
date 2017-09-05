(* OS.FileSys.chDir "./exercise/chp5" *)

(* use "../../original/base.sml"; *)
use "../../original/chp5.sml";

(* SML/NJのプリンタ設定を変える *)
val () = Control.Print.printDepth := 20  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)

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

  val fromList : Elem.T list -> Heap
  val toList : Heap -> Elem.T list
  val sort : Elem.T list -> Elem.T list
end

functor SplayHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Heap = E | T of Heap * Elem.T * Heap

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun partition (pivot, E) = (E, E)
    | partition (pivot, t as T (a, x, b)) =
        if Elem.leq (x, pivot) then
          case b of
            E => (t, E)
          | T (b1, y, b2) =>
              if Elem.leq (y, pivot) then
                let val (small, big) = partition (pivot, b2)
                in (T (T (a, x, b1), y, small), big) end
              else
                let val (small, big) = partition (pivot, b1)
                in (T (a, x, small), T (big, y, b2)) end
        else
          case a of
            E => (E, t)
          | T (a1, y, a2) =>
              if Elem.leq (y, pivot) then
                let val (small, big) = partition (pivot, a2)
                in (T (a1, y, small), T (big, x, b)) end
              else
                let val (small, big) = partition (pivot, a1)
                in (small, T (big, y, T (a2, x, b))) end

  fun insert (x, t) = let val (a, b) = partition (x, t) in T (a, x, b) end
  fun merge (E, t) = t
    | merge (T (a, x, b), t) =
        let val (ta, tb) = partition (x, t)
        in T (merge (ta, a), x, merge (tb, b)) end

  fun findMin E = raise Empty
    | findMin (T (E, x, b)) = x
    | findMin (T (a, x, b)) = findMin a
  fun deleteMin E = raise Empty
    | deleteMin (T (E, x, b)) = b
    | deleteMin (T (T (E, x, b), y, c)) = T (b, y, c)
    | deleteMin (T (T (a, x, b), y, c)) = T (deleteMin a, x, T (b, y, c))

  fun fromList nil = empty
    | fromList (x::xs) = insert (x, fromList xs)

  fun toListImpl E list = list
    | toListImpl (T (a, x, b)) list =
      let
        val list = toListImpl b list
        val list = x :: list
      in
        toListImpl a list
      end

  fun toList E = nil
    | toList t = toListImpl t nil

  fun sort list = (toList o fromList) list
end

local
  structure Elem : ORDERED =
  struct
    open Int
    type T = int
    fun eq (x : T, y) = x = y     (* delete warning of polyEqual *)
    fun lt (x, y) = x < y
    fun leq (x, y) = x <= y
  end
  structure SP = SplayHeap(Elem)
  open SP
in
val t1 = fromList [1, 2, 3, 4, 5, 6, 7, 8]
val l1 = toList t1
val t2 = fromList [8, 7, 6, 5, 4, 3, 2, 1]
val l2 = toList t2
val t3 = fromList [2, 3, 5, 1, 8, 6, 7, 4]
(*
 *      2
 *     / \
 *    1   3
 *         \
 *          5
 *         / \
 *        4   7
 *           / \
 *          6   8
 *)
val l3 = toList t3
end
