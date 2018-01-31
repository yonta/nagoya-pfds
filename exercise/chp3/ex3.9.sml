(* use "../../original/chp3.sml"; *)

(* OS.FileSys.chDir "../exercise/chp3" *)

(* SML/NJのプリンタ設定を変える *)
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)

signature ORDERED =
sig
  type T
  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature SET =
sig
  type Elem
  type Set

  val empty  : Set
  val insert : Elem * Set -> Set
  val member : Elem * Set -> bool
  val fromOrdListBad : Elem list -> Set (* ex3.9 *)
  val fromOrdList : Elem list -> Set (* ex3.9 *)
end

functor RedBlackSet (Element : ORDERED) : SET =
struct
  type Elem = Element.T

  datatype Color = R | B
  datatype Tree = E | T of Color * Tree * Elem * Tree
  type Set = Tree

  val empty = E

  fun member (x, E) = false
    | member (x, T (_, a, y, b)) =
        if Element.lt (x, y) then member (x, a)
        else if Element.lt (y, x) then member (x, b)
        else true

  fun balance (B,T (R,T (R,a,x,b),y,c),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | balance (B,T (R,a,x,T (R,b,y,c)),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | balance (B,a,x,T (R,T (R,b,y,c),z,d)) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | balance (B,a,x,T (R,b,y,T (R,c,z,d))) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | balance body = T body

  fun insert (x, s) =
        let fun ins E = T (R, E, x, E)
              | ins (s as T (color, a, y, b)) =
                  if Element.lt (x, y) then balance (color, ins a, y ,b)
                  else if Element.lt (y, x) then balance (color, a, y, ins b)
                  else s
            val T (_, a, y, b) = ins s  (* guaranteed to be non-empty *)
        in T (B, a, y, b) end

  (* ex3.9 *)
  fun fromOrdListBad nil = E
    | fromOrdListBad (x::xs) = insert (x, fromOrdListBad xs)

  (*
   * 考え方：
   * 基本的なアルゴリズムは、トーナメント状に畳み込みを行うことである。
   * 与えられたリストはすでにソートされているため、
   * リストを中心で半分に分割し、それぞれのリストからツリーを作り、
   * その2つのツリーをマージすると効率がよい。
   *
   * 例えば、[1,2,3,4,5]というリストは、
   * 再帰呼び出しによって[1,2]から作ったツリーT1と、
   * 要素3と、再帰呼び出しによって[4,5]から作ったツリーT2とを使って、
   * T(R,T1,3,T2)とマージできる。
   *
   * この実現には思いつく限り2つの方法がある。
   * 1つはリストデータに対してトーナメント状にマージをしていく方法である。
   * これはサイズとzipした要素リストを作ってから作業を行えばできる。
   * もう1つの方法はVectorへと変換して、ランダムアクセス可能にして行う方法である。
   * どちらにしても注意すべき点は、素直にリストを分割して再帰呼び出しをしようと
   * すると、リストの半分がコピーされてしまうコストが発生し、
   * 計算時間がO(n)に収まらない可能性があることである。
   *
   * 以下では、一度Vectorに変換してから、Vectorの添字を使って再帰をしている。
   *)
  local
    open Vector
  in
  fun fromOrdListImpl v i j =
      if i > j then raise Fail ("Bug:fromOrdListImpl: panic with i=" ^
                                Int.toString i ^ "j=" ^ Int.toString j ^ ".\n")
    (* vector length is 1 *)
    else if i = j then T (R, E, sub (v, i), E)
    (* vector length is 2 *)
    else if i = j - 1 then T (B, T (R, E, sub (v, i), E), sub (v, j), E)
    (* vector length is more than 2 *)
    else
      let
        val centor = i + (j - i) div 2
        val top = sub (v, centor)
        val left = fromOrdListImpl v i (centor - 1)
        val right = fromOrdListImpl v (centor + 1) j
      in
        balance (R , left, top, right)
      end

  end

  fun fromOrdList nil = E
    | fromOrdList l = fromOrdListImpl (Vector.fromList l) 0 (length l - 1)
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

  structure MyRBS = RedBlackSet (Elem)

  fun time f x =
    let
      val startT = Time.now()
      val _ = f x
      val endT = Time.now()
      val t = Time.- (endT, startT)
    in
      Time.toMicroseconds t
    end

  val tb1 = time MyRBS.fromOrdListBad (List.tabulate (100, fn x => x))
  val tb2 = time MyRBS.fromOrdListBad (List.tabulate (1000, fn x => x))
  val tb3 = time MyRBS.fromOrdListBad (List.tabulate (10000, fn x => x))
  val tb4 = time MyRBS.fromOrdListBad (List.tabulate (100000, fn x => x))
  val tb5 = time MyRBS.fromOrdListBad (List.tabulate (1000000, fn x => x))

  val t1 = time MyRBS.fromOrdList (List.tabulate (100, fn x => x))
  val t2 = time MyRBS.fromOrdList (List.tabulate (1000, fn x => x))
  val t3 = time MyRBS.fromOrdList (List.tabulate (10000, fn x => x))
  val t4 = time MyRBS.fromOrdList (List.tabulate (100000, fn x => x))
  val t5 = time MyRBS.fromOrdList (List.tabulate (1000000, fn x => x))
in
val () = print "*** Bad case [us]\n"
val () = ((print o IntInf.toString) tb1; print "\n";
          (print o IntInf.toString) tb2; print "\n";
          (print o IntInf.toString) tb3; print "\n";
          (print o IntInf.toString) tb4; print "\n";
          (print o IntInf.toString) tb5; print "\n")
val () = print "*** Good case [us]\n"
val () = ((print o IntInf.toString) t1; print "\n";
          (print o IntInf.toString) t2; print "\n";
          (print o IntInf.toString) t3; print "\n";
          (print o IntInf.toString) t4; print "\n";
          (print o IntInf.toString) t5; print "\n")
(*
 * n=1000000では両者とも計算量が増大している。
 * おそらく要素数が多すぎるためGCが走るせい？
 *)
val x = MyRBS.fromOrdList (List.tabulate (7, fn x => x))
end
