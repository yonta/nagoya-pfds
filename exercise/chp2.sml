use "../original/base.sml";
use "../original/chp2.sml";

(* OS.FileSys.chDir "../exercise" *)
(* val () = Control.lazysml := true *)

(* SML/NJのプリンタ設定を変える *)
val () = Control.Print.printDepth := 10  (* 再帰データの深さ、default : 5 *)
val () = Control.Print.printLength := 20 (* リスト長さ、default : 12 *)

(* ex 2.1 *)
fun suffixes nil = [[]]
  | suffixes (l as x::xs) = l :: suffixes xs;

(* print ex 2.1 *)
print "*** print ex 2.1\n";
suffixes [1, 2, 3, 4];

(* proof *)
(*
 * テキストの例で図示して説明する。
 *
 * もともとあったxとsuffixes xの結果は下記のようになる。
 * また、suffixes xの結果の各要素をx1~x5とする。
 *
 * x = [1,2,3,4]
 * suffixes x = [[1,2,3,4],[2,3,4],[3,4],[4],[]]
 *               x1        x2      x3    x4  x5
 *
 * すると、実行前のxのポインタと、x1~x5のポインタをテキストと同様に図示すると、
 * 今回のsuffixes実装からそれぞれのポインタ位置は下記のようになる。
 *
 *  x
 *  |
 *  v
 * [1]->[2]->[3]->[4]->null
 *  ^    ^    ^    ^   ^
 *  |    |    |    |   |
 *  x1   x2   x3   x4  x5
 *
 * よってこのsuffixesはメモリ空間を何も書き換えず、引数として与えられたリストxを
 * 戦闘から末尾まで順になぞり、ポインタを追加していくだけの動作である。
 *
 * よって、結果の生成時間は明らかにO(n)である
 *)

(* ex 2.2, 2.3*)
functor UnbalancedSet2 (Element : ORDERED) : SET =
struct
  type Elem = Element.T
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree

  val empty = E

  (* Elem * Set * Elem -> bool *)
  fun memberImpl (x, E, last) = Element.eq (x, last)
    | memberImpl (x, T (a, y, b), last) =
      if Element.lt (x, y) then memberImpl (x, a, last)
      else memberImpl (x, b, y)

  fun member (x, E) = false
    | member (x, t as T (a, y, b)) = memberImpl (x, t, y)

  exception SameElement

  fun insertImple (x, E) = T (E, x, E)
    | insertImple (x, set as T (left, elem, right)) =
      if Element.lt (x, elem) then T (insertImple (x, left), elem, right)
      else if Element.lt (elem, x) then T (left, elem, insertImple (x, right))
      else raise SameElement

  fun insert (arg as (_, set)) = insertImple arg handle SameElement => set
end;

(* print ex 2.2 *)
print "*** print ex 2.2\n";
local
  structure MyInt : ORDERED =
  struct
    type T = int
    fun eq (x : T, y) = x = y
    fun lt (x, y) = x < y
    fun leq (x, y) = x <= y
  end
  structure MyUnbalancedSet = UnbalancedSet2(MyInt);
  open MyUnbalancedSet
  fun insertN (nil, set) = set
    | insertN (h::t, set) = insertN(t, insert (h, set))
in
val testSet = insertN ([3, 2, 4, 1, 5], empty)
val b = member (4, testSet)
end;

(* print ex 2.3 *)
(* cannnot *)

(* ex 2.4 *)
functor UnbalancedSet3 (Element : ORDERED) : SET =
struct
  type Elem = Element.T
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree

  val empty = E

  (* Elem * Set * Elem -> bool *)
  fun memberImpl (x, E, last) = Element.eq (x, last)
    | memberImpl (x, T (a, y, b), last) =
      if Element.lt (x, y) then memberImpl (x, a, last)
      else memberImpl (x, b, y)

  fun member (x, E) = false
    | member (x, t as T (a, y, b)) = memberImpl (x, t, y)

  exception SameElement

  fun insertImple (x, E, last) =
    if Element.eq (x, last) then raise SameElement else T (E, x, E)
    | insertImple (x, set as T (left, elem, right), last) =
      if Element.lt (x, elem) then T (insertImple (x, left, last), elem, right)
      else T (left, elem, insertImple (x, right, elem))

  fun insert (elem, E) = T (E, elem, E)
    | insert (x, set as T (_, elem, _)) = insertImple (x, set, elem)
                                          handle SameElement => set
end

(* print ex 2.4 *)
(* cannnot *)

(* ex 2.5 (a) (b) *)
signature SET2 =
sig
  type Elem
  type Set

  val empty  : Set
  val insert : Elem * Set -> Set
  val member : Elem * Set -> bool
  val complete : Elem * int -> Set
  val create : Elem * int -> Set
end

functor UnbalancedSet4 (Element : ORDERED) : SET2 =
struct
  type Elem = Element.T
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree

  val empty = E

  (* Elem * Set * Elem -> bool *)
  fun memberImpl (x, E, last) = Element.eq (x, last)
    | memberImpl (x, T (a, y, b), last) =
      if Element.lt (x, y) then memberImpl (x, a, last)
      else memberImpl (x, b, y)

  fun member (x, E) = false
    | member (x, t as T (a, y, b)) = memberImpl (x, t, y)

  exception SameElement

  fun insertImple (x, E, last) =
    if Element.eq (x, last) then raise SameElement else T (E, x, E)
    | insertImple (x, set as T (left, elem, right), last) =
      if Element.lt (x, elem) then T (insertImple (x, left, last), elem, right)
      else T (left, elem, insertImple (x, right, elem))

  fun insert (elem, E) = T (E, elem, E)
    | insert (x, set as T (_, elem, _)) = insertImple (x, set, elem)
                                          handle SameElement => set

  fun completeImple (_, 0, lastSet) = lastSet
    | completeImple (x, depth, lastSet) =
      completeImple (x, depth - 1, T (lastSet, x, lastSet))

  fun complete (x, depth) = if depth < 0 then E
                            else completeImple (x, depth, E)

  (* size mをうけとって、m個とm+1個のツリーを作る *)
  fun create2 (x, 0, atom) = (E, atom)
    | create2 (x, size, atom) =
      let
        (* サイズが違うため、データを共有できない *)
        val subset1 = createImple (x, size, atom)
        val subset2 = createImple (x, size + 1, atom)
      in
        (subset1, subset2)
      end

  (* 任意のサイズのツリーを作る *)
  and createImple (_, 0, atom) = E
    | createImple (x, size, atom) =
    if size mod 2 = 0
    then                        (* 子が同じ数にならないとき *)
      let val (subset1, subset2) = create2 (x, size div 2 - 1, atom)
      in  T (subset1, x, subset2) end
    else                        (* 子が同じ数になるとき *)
      let val subset = createImple (x, size div 2, atom)
      in  T (subset, x, subset) end

  (* 最小単位のT (E, x, E)のデータを共有するために、引数atomで持ち回す *)
  (* おそらく処理系依存？ *)
  fun create (x, 0) = E
    | create (x, depth) = createImple (x, depth, T (E, x, E))

end;

(* print ex 2.5 (a) *)
print "*** print ex 2.5 (a)\n";
local
  structure MyInt : ORDERED =
  struct
    type T = int
    fun eq (x : T, y) = x = y
    fun lt (x, y) = x < y
    fun leq (x, y) = x <= y
  end
  structure MyUnbalancedSet = UnbalancedSet4(MyInt);
  open MyUnbalancedSet
in
val _ = complete (5, 2);
val _ = create (5, 4);
end

(* ex 2.6 *)
functor FiniteMap (K : ORDERED) : FINITE_MAP =
struct
  type Key = K.T
  datatype 'a Map =
           E
         | T of 'a Map * (Key * 'a) * 'a Map

  val empty = E

  fun bind (key, x, E) = T (E, (key, x), E)
    | bind (key, x, tree as T (left, elem as (k, v), right)) =
      if K.eq (key, k) then tree
      else if K.lt (key, k) then T (bind (key, x, left), elem, right)
      else T (left, elem, bind (key, x, right))

  fun lookup (key, E) = raise NotFound
    | lookup (key , T (left, (k, v), right )) =
      if K.eq (key, k) then v
      else if K.lt (key, k) then lookup (key, left)
      else lookup (key, right)
end

(* print ex 2.6 *)
local
  structure MyMap = FiniteMap(MyInt);
  open MyMap
  val map = empty
  val map = bind (3, "3", map)
  val map = bind (2, "2", map)
  val map = bind (4, "4", map)
  val map = bind (1, "1", map)
in
val map = bind (5, "5", map);
val value = lookup (5, map)
end
