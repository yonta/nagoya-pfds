(* OS.FileSys.chDir "./exercise/chp5" *)

(* use "../../original/base.sml"; *)
use "../../original/chp5.sml";

(* SML/NJのプリンタ設定を変える
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)
*)

datatype Tree = E | T of Tree * int * Tree

fun bigger (pivot, E) = E
  | bigger (pivot, T (a, x, b)) =
    if x <= pivot then bigger (pivot, b)
    else case a of
             E => T (E, x, b)
           | T (a1, y, a2) =>
             if y <= pivot then T (bigger (pivot, a2), x, b)
             else T (bigger (pivot, a1), y, T (a2, x, b))

(* ex 5.4 *)
fun smaller (pivot, E) = E
  | smaller (pivot, T (a, x, b)) =
    if pivot < x then smaller (pivot, a)
    else case b of
             E => T (a, x, E)
           | T (b1, y, b2) =>
             if pivot < y then T (a, x , smaller (pivot, b1))
             else T (T (a, x, b1), y, smaller (pivot, b2))

(* ex 5.4 test *)
val tb = T (T (T (E, 1, E), 2, E), 3, E)
val tbb = bigger (0, tb)
val ts = T (E, 1, T (E, 2, T (E, 3, E)))
val tss = smaller (4, ts)
