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

use "../../original/chp4.sml";  (* Streamモジュール *)

signature ORDERED =
sig
  type T

  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature SORTABLE =
sig
  structure Elem : ORDERED

  type Sortable

  val empty : Sortable
  val add   : Elem.T * Sortable -> Sortable
  val sort  : Sortable -> Elem.T list

  (* ex6.7.b *)
  val takeSorted : Sortable * int -> Elem.T list
end

functor NewBottomUpMergeSort (Element : ORDERED) : SORTABLE =
struct

  local
    open Stream
  in

  structure Elem = Element

  type Sortable = int * Elem.T Stream.Stream list

  (* 'a Strem * 'a Stream -> 'a Stream *)
  fun lazy mrg ($Nil, ys) = ys
         | mrg (xs, $Nil) = xs
         | mrg (xs as $(Cons (x, xs')), ys as $(Cons (y, ys'))) =
           if Elem.leq (x, y) then $(Cons (x, mrg (xs', ys)))
           else $(Cons (y, mrg (xs, ys')))

  val empty = (0, [])

  fun add (x, (size, segs)) =
    let
      (* 'a Stream * 'a Stream list * int -> 'a Stream list *)
      fun addSeg (seg, segs, size) =
        if size mod 2 = 0 then seg :: segs
        else addSeg (mrg (seg, hd segs), tl segs, size div 2)
      val single = $(Cons (x, $Nil))
    in
      (size + 1, addSeg (single, segs, size))
    end

  fun streamToList ($Nil) = nil
    | streamToList ($(Cons (x, stream))) = x :: streamToList stream

  fun sort (_, segs) =
    let val stream = foldl mrg ($Nil) segs
    in  streamToList stream end

  (* val takeSorted : Sortable * int -> Elem.T list *)
  fun takeSorted ((0, nil), 0) = nil
    | takeSorted ((size, s), n) =
      if n > size then raise Subscript
      else
        let
          (* mrgがlazyなので、各要素は評価されない *)
          val stream = foldl mrg ($Nil) s
          (* Stream.takeもlazyなので、各要素は評価されない *)
          val stream = take (n, stream)
        in
          (* take対象の要素のみ評価する *)
          streamToList stream
        end

  end (* local *)
end (* struct *)

(* example *)
local
  structure Element : ORDERED =
  struct
    open Int
    type T = int
    fun eq (x : int, y) = x = y
    fun lt (x, y) = x < y
    fun leq (x, y) = x <= y
  end
  structure BM = NewBottomUpMergeSort (Element)
  open BM
  val e0 = BM.empty
  val elems = [7, 3, 2, 9, 6, 8, 1, 4, 5, 10]
in
val e1 = foldl add empty elems  (* 10要素のSortable *)
val l1 = sort e1
val l1' = takeSorted (e11, 5)
end
