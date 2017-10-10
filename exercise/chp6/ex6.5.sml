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

(* 実装を試してみたが、lazy化されてるかチェックできない。
 * ヒープ構造がlazyだが、ユーザはElem要素しか選択できないため、
 * 重たい処理をいれることができない。
 *)
local
  structure Elem : ORDERED =
  struct
    open Int
    type T = int
    fun eq (x : T, y) = x = y
    fun lt (x, y) = x < y
    fun leq (x, y) = x <= y
  end
  structure LBH = LazyBinomialHeap (Elem)
  structure SH = SizedHeap (LBH)
  open SH
  val e1 = insert (0, empty)
  (* 2^29サイズのヒープをつくる *)
  val en = foldl
             (fn (_, merged) => merge (merged, merged))
             e1
             (List.tabulate (29, fn _ => ()))
in
val enBefore = en
val t = isEmpty en
val enAfter1 = en
val enAfter2 = deleteMin en
end
