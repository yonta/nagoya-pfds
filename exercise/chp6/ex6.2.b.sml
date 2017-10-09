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

local
  open Stream
in

structure BankersQueue2 : QUEUE =
struct
  type 'a Queue = int * 'a Stream * int * 'a Stream

  val empty = (0, $Nil, 0, $Nil)
  fun isEmpty (lenf, _, _, _) = (lenf = 0)

  (* diff: 制約条件を2倍に *)
  fun check (q as (lenf, f, lenr, r)) =
        if lenr <= 2 * lenf then q else (lenf+lenr, f ++ reverse r, 0, $Nil)

  fun snoc ((lenf, f, lenr, r), x) = check (lenf, f, lenr+1, $(Cons (x, r)))

  fun head (_, $Nil, _, _) = raise Empty
    | head (_, $(Cons (x, _)), _, _) = x

  fun tail (_, $Nil, _, _) = raise Empty
    | tail (lenf, $(Cons (_, f')), lenr, r) = check (lenf-1, f', lenr, r)
end

end

local
  fun repeat 0 _ initArg = initArg
    | repeat n f initArg = repeat (n - 1) f (f initArg)
  (* 100回では1ns未満の実行時間になり、TimeやTimerモジュールで調査できない
   * そのため、1000回のsnoc/tailで実行する *)
  fun bench empty snoc tail =
    let
      val q = repeat 1000 (fn q => snoc (q, 0)) empty
      val q = repeat 1000 tail q
    in
      ()
    end

  structure BQ = BankersQueue
  fun bench1 () = bench BQ.empty BQ.snoc BQ.tail

  structure BQ2 = BankersQueue2
  fun bench2 () = bench BQ2.empty BQ2.snoc BQ2.tail

  (* ベンチマークをn回まわし、合計タイムを返す *)
  fun repeatBench 0 f time = time
    | repeatBench n f time =
      let
        val cputimer = Timer.startCPUTimer ();
        val () = f ()
        val resultTime = #usr (Timer.checkCPUTimer cputimer)
      in
        repeatBench (n - 1) f (Time.+ (time, resultTime))
      end

  fun benchmark target name =
    let
      (* 1000回まわして平均値を取る *)
      val n = 1000
      val resultTime = repeatBench n target Time.zeroTime
      val resultReal = Time.toReal resultTime / Real.fromInt n * 1000.0 (* ns *)
      val result =
          "*** Benchmark of " ^ name ^ " : " ^
          Real.toString resultReal ^
          " [ns]\n"
    in
      print result
    end
in
val () = benchmark bench1 "Bankersqueue "
val () = benchmark bench1 "Bankersqueue2"
end
