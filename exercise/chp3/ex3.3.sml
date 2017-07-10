use "../../original/base.sml";
use "../../original/chp3.sml";

(* OS.FileSys.chDir "../exercise/chp3" *)

(* SML/NJのプリンタ設定を変える *)
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)

structure Elem : ORDERED =
struct
  open Int
  type T = int
  fun eq (x : T, y) = x = y     (* delete warning of polyEqual *)
  fun lt (x, y) = x < y
  fun leq (x, y) = x <= y
end

datatype Heap = E | T of int * Elem.T * Heap * Heap

fun rank E = 0
  | rank (T (r,_,_,_)) = r

fun makeT (x, a, b) = if rank a >= rank b then T (rank b + 1, x, a, b)
                      else T (rank a + 1, x, b, a)

fun merge (h, E) = h
  | merge (E, h) = h
  | merge (h1 as T (_, x, a1, b1), h2 as T (_, y, a2, b2)) =
    if Elem.leq (x, y) then makeT (x, a1, merge (b1, h2))
    else makeT (y, a2, merge (h1, b2))

fun toHeapWithSize x = (T (1, x, E, E), 1)

(* Heap * int list -> (Heap * int) list *)
(*
 * example
 *                       merge             recursive call
 *   [ (1), (2), (3, 4) ] -> [ (1, 2), (3, 4) ] -> [ (1, 2, 3, 4) ]
 *)
fun mergeStack nil = nil
  | mergeStack [x] = [x]
  | mergeStack (stack as (heap1, size1) :: (heap2, size2) :: t) =
    if size1 < size2 then stack
    else
      let
        val heap = merge (heap1, heap2)
        val size = size1 + size2
        val stack = (heap, size) :: t
      in
        mergeStack stack
      end

(* Heap list -> Heap *)
(*
 * Heap listの要素を全てマージする
 *)
fun mergeAllStack nil = E
  | mergeAllStack [x] = x
  | mergeAllStack (h1::h2::t) = mergeAllStack (merge (h1, h2) :: t)

(* Elem.T list -> Heap list -> Heap *)
(*
 * example
 *   ()はHeapを表すとする
 *                     toHeapWithsize           recursive call
 *   [1, 2, 3, 4, 5] * [] -> [2, 3, 4, 5] * [(1)] ->
 *                        mergeStack                        recursive call
 *   [3, 4, 5] * [(2), (1)] ->           [3, 4, 5] * [(1, 2)] ->
 *
 *   ...
 *                          mergeAllStack
 *   [] * [(5), (1, 2, 3, 4)] ->            (1, 2, 3, 4, 5)
 *)
fun toListImpl nil stack = mergeAllStack (map #1 stack)
  | toListImpl (h::t) stack =
    let
      val heap = toHeapWithSize h
      val stack = heap :: stack
      val stack = mergeStack stack
    in
      toListImpl t stack
    end

(* Elem.T list -> Heap *)
fun fromList nil = E
  | fromList list = toListImpl list nil
