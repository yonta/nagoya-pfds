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

(*
 * SML/NJの$を使うため、base.smlで再定義されている$とforceを使わない。
 * むしろbase.smlを使うと、型推論エラーでchp4.smlの例がまったく動かない。
 *)
(* use "../../original/base.sml"; *)
use "../../original/chp4.sml";
use "../../original/chp6.sml";

functor SizedHeap (Heap : HEAP) : HEAP =
struct
  structure Elem = Heap.Elem
  type Heap = int * Heap.Heap

  val empty = (0, Heap.empty)

  (* isEmptyはsize部しか見ず、heapの実体がlazyでも評価しない *)
  fun isEmpty (0, _) = true
    | isEmpty _ = false

  fun insert (elem, (size, heap)) = (size + 1, Heap.insert (elem, heap))

  fun merge ((size1, heap1), (size2, heap2)) =
    (size1 + size2, Heap.merge(heap1, heap2))

  fun findMin (0, _) = raise Empty
    | findMin (_, heap) = Heap.findMin heap

  fun deleteMin (0, _) = raise Empty
    | deleteMin (size, heap) = (size - 1, Heap.deleteMin heap)
end
