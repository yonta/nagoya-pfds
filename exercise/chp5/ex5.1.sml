(* OS.FileSys.chDir "./exercise/chp5" *)

(* use "../../original/base.sml"; *)
(* use "../../original/chp5.sml"; *)

(* SML/NJのプリンタ設定を変える
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)
 *)

signature DEQUE =
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val cons    : 'a * 'a Queue -> 'a Queue
  val head    : 'a Queue -> 'a
  val tail    : 'a Queue -> 'a Queue

  val snoc    : 'a Queue * 'a -> 'a Queue
  val last    : 'a Queue -> 'a
  val init    : 'a Queue -> 'a Queue
end

(*
 *                   f     r
 *                ------------
 *  head * tail <-      |       <- snoc
 *  cons        ->      |       -> last * init
 *                ------------
 *
 * 条件1: fが空かつrが2要素以上なら、rを半分にして新しいfとrを作る
 * 条件2: rが空かつfが2要素以上なら、fを半分にして新しいfとrを作る
 *)
(* ex5.1 (a) *)
structure Deque : DEQUE =
struct
  type 'a Queue = 'a list * 'a list

  val empty = (nil, nil)

  fun isEmpty (nil, nil) = true
    | isEmpty _ = false

  fun splitAtImpl 0 toList fromList = (rev toList, fromList)
    | splitAtImpl i toList (h::t) = splitAtImpl (i-1) (h::toList) t
    | splitAtImpl _ _ nil =
      raise Fail "Deque.splitAtImpl: call with nil in 3rd arguments"

  fun splitAt n list = splitAtImpl n nil list

  fun half nil = empty
    | half l =
      let
        val i = length l div 2
      in
        splitAt i l
      end

  fun check (f as _::_::_, nil) =
      let val (f, r) = half f
      in  (f, rev r) end
    | check (nil, r as _::_::_) =
      let val (r, f) = half r
      in  (rev f, r) end
    | check q = q

  fun cons (x, (f, r)) = check (x::f, r)

  fun head ((x::_, _)) = x
    | head (nil, [x]) = x
    | head _ = raise Empty

  fun tail (_::f, r) = check (f, r)
    | tail (nil, [x]) = empty
    | tail _ = raise Empty

  fun snoc ((f, r), x) = check (f, x::r)
  (* fun snoc ((nil, r), x) = check (nil, x::r) *)
  (*   | snoc ((f, r), x) = (f, x::r) *)

  fun last (_, x::_) = x
    | last ([x], nil) = x
    | last _ = raise Empty

  fun init (f, _::r) = check (f, r)
    | init ([x], nil) = empty
    | init _ = raise Empty
end

local
  open Deque
in
val d0 = empty
val d1 = cons (0, d0)
val d2 = cons (1, d1)
val d3 = cons (2, d2)
val d4 = snoc (d3, ~1)
val d5 = init d4
val d6 = cons (3, d5)
val d7 = init d6
val d = init d1
end

(*
 * ex5.1 (b)
 *
 * 証明
 *
 * Dequeへの各操作cons、head/tail、snoc、last/initを行い、もとのDeque
 * （f、rの組とする）から新たなDeque（f'、r'の組とする）が作られるとする。
 * このとき、Dequeの各操作4つがO(1)償却時間であることを証明する。
 *
 * このとき、Dequeは入出力方向に対して対称であるため、|f|>=|r|のパターンを
 * 証明すれば、Dequeのすべてのパターンについて証明したことになる。
 *
 * 1) cons
 * |f|=0 かつ |r|=0 のときは、エラーなので証明しなくてよい
 * |f|>0のとき、操作はfに1つ要素を足すのでコストは1である。
 * また、ポテンシャルの変化は、
 * Φ(f',r')- Φ(f,r) = abs(|f'|-|r'|) - abs(|f|-|r|)
 *                  = abs(|f|+1-|r|) - abs(|f|-|r|)
 *                  = 1
 * よって、償却コストは1+1=2である。
 * よって、定数時間である。
 *
 * 2) head/tail
 * このとき、tailはcheckを呼び出すが、Dequeの不変条件と|f|>=|r|より、
 * checkは何もしないためコストは0のため考慮しなくてよい。
 * consと同様に、 |f|=0 かつ |r|=0 のときは、エラーなので証明しなくてよい
 * |f|>0 のとき、操作はfから1つ要素を取り除くのでコストは1である。
 * また、ポテンシャルの変化は、
 * Φ(f',r')- Φ(f,r) = abs(|f'|-|r'|) - abs(|f|-|r|)
 *                  = abs(|f|-1-|r|) - abs(|f|-|r|)
 * このとき、|f|!=|r|の場合、ポテンシャルの変化は-1であり、
 * 償却コストは1-1で0である。
 * また|f|=|r|、すなわちabs(|f|-|r|)=0のときは、ポテンシャルの変化は1であり、
 * 償却コストは1+1で2である。
 * よって、定数時間である。
 *
 * 3) snoc
 * consと同様に、 |f|=0 かつ |r|=0 のときは、エラーなので証明しなくてよい
 * |f|>0のとき、操作はfに1つ要素を足すのでコストは1である。
 * また、ポテンシャルの変化は、
 * Φ(f',r')- Φ(f,r) = abs(|f'|-|r'|) - abs(|f|-|r|)
 *                  = abs(|f|-(|r|+1)) - abs(|f|-|r|)
 * このとき、|f|!=|r|の場合、ポテンシャルの変化は-1であり、
 * 償却コストは1-1=0である。
 * また|f|=|r|、すなわちabs(|f|-|r|)=0のときは、ポテンシャルの変化は1であり、
 * 償却コストは1+1=2である。
 * よって、定数時間である。
 *
 * 4) last/init
 * |f|と|r|のサイズで場合分けをする。
 *
 * |r|=0のとき
 * |f|=0の場合はエラーのため証明しなくてよい。
 * Dequeの定義より|f|=1のとき、操作コストが1、ポテンシャル変化が1のため、
 * 償却コストは2である。
 * |f|>1は、Dequeの不変条件よりありえない。
 *
 * |r|>1のとき
 * このとき、lastはcheckを呼び出すが、|r|の条件より何もしないため考慮しない。
 * すると、操作コストは1、ポテンシャル変化は1のため、償却コストは2である。
 *
 * |r|=1かつ|f|=1のとき
 * このとき、lastはcheckを呼び出すが、条件より何もしないため考慮しない。
 * すると、操作コストは1、ポテンシャル変化は1のため、償却コストは2である。
 *
 * |r|=1かつ|f|>1のとき
 * このときは、lastが呼び出したcheckが操作を行う。
 * このcheckはfの先頭半分をf'にし、残った部分をreverseしてr'とする。
 * よって、f'の作成にははコピーコストが、r'の作成にはreverseのコストがかかり、
 * この操作全体にかかる最小のコストは|f|となる。
 * このとき、ポテンシャルの変化は、
 * Φ(f',r')- Φ(f,r) = abs(|f'|-|r'|) - abs(|f|-|r|)
 *                  = abs(|f'|-|r'|) - abs(|f|-1)
 *                  = abs(|f'|-|r'|) - |f| - 1
 * このとき、abs(|f'|-|r'|)は|f|によって決定され、|f| div 2に等しいため、
 * 0か1である。
 * なので、ポテンシャルの変化は少なくとも1-|f|-1=-|f|だけ変化する。
 * つまり、償却コストは、|f|-|f|=0である。
 * よって、定数時間である。
 *)
