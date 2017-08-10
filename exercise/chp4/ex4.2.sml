(* OS.FileSys.chDir "../exercise/chp4" *)

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

(* SML/NJのプリンタ設定を変える *)
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)

(*
 * lazyの機能であるsuspが動いているかのテスト
 * fib 40くらいで重たい処理になる
fun fib (0 | 1) = 1
  | fib n = fib (n-1) + fib (n-2)

fun lazy addfib n stream = $(Stream.Cons (fib n, stream))
(* 早い！！！、これが遅かったらlazy処理が効いていない *)
val x = addfib 40 ($Stream.Nil)
(* 遅い！！！ *)
val y = case force x of
            Stream.Cons (fib40, stream) => fib40
          | Stream.Nil => 0                      (* ありえないパターン *)
(* 早い！！！ *)
val y = case force x of
            Stream.Cons (fib40, stream) => fib40
          | Stream.Nil => 0                      (* ありえないパターン *)
*)

(* ex4.2 *)
signature ORDERED =
sig
  type T
  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature STREAM =
sig
  type elem
  datatype StreamCell = Nil | Cons of elem * Stream
  withtype Stream = StreamCell susp

  val ++      : Stream * Stream -> Stream  (* stream append *)
  val take    : int * Stream -> Stream
  val drop    : int * Stream -> Stream
  val reverse : Stream -> Stream
  val sort    : Stream -> Stream (* ex4.2 *)
end

functor MakeStream (Elem : ORDERED) : STREAM =
struct
  type elem = Elem.T
  datatype StreamCell = Nil | Cons of elem * Stream
  withtype Stream = StreamCell susp

  fun lazy ($Nil) ++ t = t
         | ($(Cons (x, s))) ++ t = $(Cons (x, s ++ t))

  fun lazy take (0, s) = $Nil
         | take (n, $Nil) = $Nil
         | take (n, $(Cons (x, s))) = $(Cons (x, take (n-1, s)))

  fun lazy drop (n, s) =
    let
      fun drop' (0, s) = s
        | drop' (n, $Nil) = $Nil
        | drop' (n, $(Cons (x, s))) = drop' (n-1, s)
    in
      drop' (n, s)
    end

  fun lazy reverse s =
    let
      fun reverse' ($Nil, r) = r
        | reverse' ($(Cons (x, s)), r) = reverse' (s, $(Cons (x, r)))
    in
      reverse' (s, $Nil)
    end

  (* ex4.2 *)
  fun lazy insert x (s as $Nil) = $(Cons (x, s))
         | insert x (s as $(Cons (y, s1))) =
           $(if Elem.lt (x, y) then Cons (x, s) else Cons (y, insert x s1))

  fun lazy sort ($Nil) = $Nil
         | sort ($(Cons (x, s))) = insert x (sort s)
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
  structure MyStream = MakeStream (Elem)
  open MyStream
  fun fromList nil = $Nil
    | fromList (x::xs) = $(Cons (x, fromList xs))
  local
    fun printStreamImpl ($Nil) = print "]\n"
      | printStreamImpl ($(Cons (x, s))) =
        (print (Int.toString x);
         print ", ";
         printStreamImpl s)
  in
  fun printStream s = (print "[ "; printStreamImpl s)
  end
in
val s = fromList [ 1, 3, 4, 2, 6, 5, 0 ]
val s = sort s
val s = take (3, s)
val () = printStream s
end

(* 証明 *)
(*
 * size s = nのとき、take (k, sort s)のコストがn^2でなくknなのを示す
 *
 * takeする回数kに関する帰納法で示す。
 *
 * k=0のとき、sortによる比較は行われず、コストは明らかに0である。
 *
 * kのときを考える。
 * take (k, S)は実装より、S:'a Streamから(k-1)個をtakeして、もう1個takeする。
 * ここで帰納法の仮定より、Sから(k-1)個takeするコストは(k-1)nである。
 * この次のn回目のtakeは、(k-1)takeされて残ったSから最小のものを見つけ
 * だすことになる。
 * このコストは、たかだかnである。
 * ただし、takeやsortがまともな遅延評価を実装しているとする。
 * よって、コストは(k-1)n + n = knである。
 *)
(*
 * force (take (k, sort list))のforceの呼び出し回数をカウントする証明
 *)
