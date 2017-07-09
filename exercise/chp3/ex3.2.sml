use "../../original/base.sml";
use "../../original/chp3.sml";

(* OS.FileSys.chDir "../exercise/chp3" *)

(* SML/NJのプリンタ設定を変える *)
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)


structure Element : ORDERED =
struct
  open Int
  type T = int
  fun eq (x : T, y) = x = y     (* delete warning of polyEqual *)
  fun lt (x, y) = x < y
  fun leq (x, y) = x <= y
end

datatype Heap = E | T of int * Element.T * Heap * Heap

fun insert (x, E) = T (1, x, E, E)
  | insert (x, heap as T (rank, elem, left, right)) =
    if Element.leq (x, elem) then T (1, x, heap, E)
    else T (rank, elem, insert (x, left), right)
