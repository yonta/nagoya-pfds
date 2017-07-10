use "../../original/base.sml";
use "../../original/chp3.sml";

(* OS.FileSys.chDir "../exercise/chp3" *)

(* SML/NJのプリンタ設定を変える *)
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)

(* (a) *)
(*
 * Proof.
 *   補題としてrank L >= rank Rから証明したsize L >= size Rの定義を含むので、
 *   明らかに同様の証明ができる
 *)

functor WeightBiasedLeftistHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Heap = E | T of int * Elem.T * Heap * Heap

  fun size E = 0
    | size (T (s,_,_,_)) = s

  fun makeT (x, h1, h2) =
    let
      val s1 = size h1
      val s2 = size h2
    in
      if s1 >= s2 then T (s1 + s2, x, h1, h2) else T (s1 + s2, x, h2, h1)
    end

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (h, E) = h
    | merge (E, h) = h
    | merge (h1 as T (_, x, a1, b1), h2 as T (_, y, a2, b2)) =
        if Elem.leq (x, y) then makeT (x, a1, merge (b1, h2))
        else makeT (y, a2, merge (h1, b2))

  fun newMerge (h, E) = h
    | newMerge (E, h) = h
    | newMerge (h1 as T (s1, x, a1, b1), h2 as T (s2, y, a2, b2)) =
      let
        val s = s1 + s2
      in
        if Elem.leq (x, y)
        then
          if s1 - 1 >= s2 then T (s, x, newMerge (a1, b1), h2)
          else T (s, x, h2, newMerge(a1, b1))
        else
          if s1 >= s2 - 1 then T (s, y, h1, newMerge (a2, b2))
          else T (s, y, newMerge (a2, b2), h1)
      end

  val merge = newMerge

  fun insert (x, h) = merge (T (1, x, E, E), h)

  fun findMin E = raise Empty
    | findMin (T (_, x, a, b)) = x
  fun deleteMin E = raise Empty
    | deleteMin (T (_, x, a, b)) = merge (a, b)
end

structure Elem : ORDERED =
struct
  open Int
  type T = int
  fun eq (x : T, y) = x = y     (* delete warning of polyEqual *)
  fun lt (x, y) = x < y
  fun leq (x, y) = x <= y
end
