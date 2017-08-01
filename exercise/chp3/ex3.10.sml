use "../../original/chp3.sml";

(* OS.FileSys.chDir "../exercise/chp3" *)

(* SML/NJのプリンタ設定を変える *)
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)

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

  (* ex3.10.a *)
  fun lbalance (B,T (R,T (R,a,x,b),y,c),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | lbalance (B,T (R,a,x,T (R,b,y,c)),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | lbalance body = T body

  (* ex3.10.a *)
  fun rbalance (B,a,x,T (R,T (R,b,y,c),z,d)) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | rbalance (B,a,x,T (R,b,y,T (R,c,z,d))) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | rbalance body = T body

  (* ex3.10.a *)
  fun insert (x, s) =
    let fun ins E = T (R, E, x, E)
          | ins (s as T (color, a, y, b)) =
            if Element.lt (x, y) then lbalance (color, ins a, y ,b)
            else if Element.lt (y, x) then rbalance (color, a, y, ins b)
            else s
        val T (_, a, y, b) = ins s  (* guaranteed to be non-empty *)
    in T (B, a, y, b) end
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

  (* ex3.10.b *)
  fun llbalance (B,T (R,T (R,a,x,b),y,c),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | llbalance body = T body

  (* ex3.10.b *)
  fun lrbalance (B,T (R,a,x,T (R,b,y,c)),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | lrbalance body = T body

  (* ex3.10.b *)
  fun rlbalance (B,a,x,T (R,T (R,b,y,c),z,d)) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | rlbalance body = T body

  (* ex3.10.b *)
  fun rrbalance (B,a,x,T (R,b,y,T (R,c,z,d))) = T (R,T (B,a,x,b),y,T (B,c,z,d))
    | rrbalance body = T body

  (* ex3.10.b *)
  (*
   * ポイント：
   * lbalanceを呼び出して左サブツリーへ挿入するときを考える。
   * この次の再帰呼び出しされたinsにおいて、また左サブツリーへ挿入するとする。
   * lbalanceでの検査はサブツリーで左側へinsしたことがわかっている。
   * よって、LLを検査するだけでよく、LRを検査する必要はない。
  fun insert (x, s) =
        let fun ins E = T (R, E, x, E)
              | ins (s as T (color, a, y, b)) =
                if Element.lt (x, y) then lbalance (color, ins a, y ,b)
                else if Element.lt (y, x) then rbalance (color, a, y, ins b)
                else s
            val T (_, a, y, b) = ins s  (* guaranteed to be non-empty *)
        in T (B, a, y, b) end
  *)

  (* ex3.10.b *)
  datatype direction = Left | Right | None

  (*
   * ex3.10.b
   * val ins : Tree -> Tree * direction
   *                   再帰呼び出しでどちらに挿入したかを返す
   *)
  fun ins E x = (T (R, E, x, E), None)
    | ins (s as T (color, a, y, b)) x =
      if Element.lt (x, y)      (* insert left *)
      then
        let
          (* 左挿入、次の挿入はdireciton側に挿入 *)
          val (left, direction) = ins a x
        in
          case direction of
            (* Left-Leftへ挿入 *)
              Left => (llbalance (color, left, y, b), Left)
            (* Left-Rightへ挿入 *)
            | Right => (lrbalance (color, left, y, b), Left)
            (* Left-Eへ挿入 *)
            | None => (T (color, left, y, b), Left)
        end
      else if Element.lt (y, x) (* insert right *)
      then
        let
          (* 右挿入、次の挿入はdirection側に挿入 *)
          val (right, direction) = ins b x
        in
          case direction of
            (* Right-Leftへ挿入 *)
              Left => (rlbalance (color, a, y, right), Right)
            (* Right-Rightへ挿入 *)
            | Right => (rrbalance (color, a, y, right), Right)
            (* Right-Eへ挿入 *)
            | None => (T (color, a, y, right), Right)
        end
      else (s, None)

  (* ex3.10.b *)
  fun insert (x, s) =
    let val (T (_, a, y, b), _) = ins s x  (* guaranteed to be non-empty *)
    in T (B, a, y, b) end
end

local
  structure Elem : ORDERED =
  struct
    open Int
    type T = int
    fun eq (x : T, y) = x = y   (* to delete warning of polyEqual *)
    fun lt (x, y) = x < y
    fun leq (x, y) = x <= y
  end

  structure MyRBS = RedBlackSet (Elem)
in
  val t0 = MyRBS.empty
  val t1 = MyRBS.insert (0, t0)
  val t2 = MyRBS.insert (1, t1)
  val t3 = MyRBS.insert (2, t2)
  val t4 = MyRBS.insert (3, t3)
  val t5 = MyRBS.insert (4, t4)
  val t6 = MyRBS.insert (5, t5)
  val t7 = MyRBS.insert (7, t6) (* すごくバランスされる！ *)
end
