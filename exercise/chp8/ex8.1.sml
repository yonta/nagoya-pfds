(* OS.FileSys.chDir "../exercise/chp8" *)

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
  val delete : Elem * Set -> Set (* ex8.1 *)
end

(* ex8.1 *)
functor RedBlackSet (Element : ORDERED) :> SET where type Elem = Element.T =
struct
  type Elem = Element.T

  datatype color = R | B
  (* elemフィールドは、Tが持つ要素の値と、
   * その値がdeleteによって削除フラグがつけられたかを表す。
   * 削除フラグがfalseなら、値がdeleteによって削除されている。
   * このノードは、未来の一括再構築で削除される。 *)
  datatype tree = E
                | T of node
  withtype node = {color : color, elem : Elem * bool, left : tree, right : tree}
  (* sizeフィールドはSet内のTreeのおおよその要素数を表す。
   * deadフィールドはSet内のTreeの要素の内、
   * 削除フラグがつけられたおおよその要素数を表す。
   *
   * insert/delete関数は、呼び出されても挿入/削除しない場合があるため、
   * sizeとdeadはおおよその数を表す。
   *
   * size >= dead*2 が不変条件であり、この条件が崩れたら一括再構築をする *)
  type Set = {size : int, dead : int, tree : tree}

  val empty = {size = 0, dead = 0, tree = E}

  fun memberTree (x, E) = false
    | memberTree (x, T {elem = (elem, live), left, right, ...} : tree) =
      if Element.lt (x, elem) then memberTree (x, left)
      else if Element.lt (x, elem) then memberTree (x, right)
      else live
  fun member (x, {tree, ...} : Set) = memberTree (x, tree)

  (* balance : node -> tree、P.35の図に合わせて名前付けしている *)
  fun balance {color = B, elem = z, right = d,
               left = T {color = R, elem = y, right = c,
                         left = T {color = R, elem = x, left = a, right = b}}} =
      T {color = B, elem = y,
         left = T {color = B, elem = x, left = a, right = b},
         right = T {color = B, elem = z, left = c, right = d}}
    | balance {color = B, elem = z, right = d,
               left = T {color = R, elem = x, left = a,
                         right= T {color = R, elem = y, left = b, right = c}}} =
      T {color = B, elem = y,
         left = T {color = B, elem = x, left = a, right = b},
         right = T {color = B, elem = z, left = c, right = d}}
    | balance {color = B, elem = x, left = a,
               right = T {color = R, elem = z, right = d,
                          left= T {color = R, elem = y, left = b, right = c}}} =
      T {color = B, elem = y,
         left = T {color = B, elem = x, left = a, right = b},
         right = T {color = B, elem = z, left = c, right = d}}
    | balance {color = B, elem = x, left = a,
               right = T {color = R, elem = y, left = b,
                          right= T {color = R, elem = z, left = c, right = d}}}=
      T {color = B, elem = y,
         left = T {color = B, elem = x, left = a, right = b},
         right = T {color = B, elem = z, left = c, right = d}}
    | balance body = T body

  fun insertTree (x, E) =
      T {color = R, elem = (x, true), left = E, right = E}
    | insertTree (x, t as T {color, elem = elem as (y, live), left, right}) =
      if Element.lt (x, y)
      then balance {color = color, elem = elem, right = right,
                    left = insertTree (x, left)}
      else if Element.lt (y, x)
      then balance {color = color, elem = elem, left = left,
                    right = insertTree (x, right)}
      else if live then raise Fail "Already inserted" (* 新たな木を作らない *)
      else T {color = color, elem = (y, true), left = left, right = right}
  fun insert (x, set as {size, dead, tree}) =
      {size = size + 1, dead = dead, tree = insertTree (x, tree)}
      handle Fail mess => set

  (* toOrdList : tree -> elem list
   * ツリー内の削除フラグがついてないものだけを対象として、
   * ソート済みリストへ変換する *)
  fun toOrdListImpl E l = l
    | toOrdListImpl (T {elem = elem as (_, live), left, right, ...} : tree) l =
      let
        val l = toOrdListImpl right l
        val l = if live then elem :: l else l
      in
        toOrdListImpl left l
      end
  fun toOrdList t = toOrdListImpl t nil

  (* fromOrdList : elem list -> tree、ソート済みリストをツリーへ変換する
   * 実装内容はex3.9より。 *)
  local
    open Vector
  in
  fun fromOrdListImpl v i j =
      if i > j then raise Fail ("Bug:fromOrdListImpl: panic with i=" ^
                                Int.toString i ^ "j=" ^ Int.toString j ^ ".\n")
    (* vector length is 1 *)
    else if i = j then T {color = R, elem = sub (v, i), left = E, right = E}
    (* vector length is 2 *)
    else if i = j - 1 then T {color = B, elem = sub (v, j), right = E,
                              left = T {color = R, elem = sub (v, i),
                                        left = E, right = E}}
    (* vector length is more than 2 *)
    else
      let
        val center = i + (j - i) div 2
        val top = sub (v, center)
        val left = fromOrdListImpl v i (center - 1)
        val right = fromOrdListImpl v (center + 1) j
      in
        balance {color = R, elem = top, left = left, right = right}
      end
  end
  fun fromOrdList nil = E
    | fromOrdList l = fromOrdListImpl (Vector.fromList l) 0 (length l - 1)

  (* batchedRebuild : tree -> int * tree、一括再構築を行う。
   * アルゴリズムとしては、削除フラグがついてないものだけをソート済みリストに
   * 一度出力して、そのリストからツリーを作り直す。
   * toOrdListもlengthもfromOrdListも、最悪計算時間はO(n)なので、
   * この関数の最悪計算時間O(n)である。 *)
  fun batchedRebuild E = (0, E)
    | batchedRebuild t =
      let
        val l = toOrdList t
        val size = length l
        val t = fromOrdList l
      in
        (size, t)
      end

  (* 不変条件size >= dead*2をチェックして、満たしてないときは一括再構築する。 *)
  fun check (set as {size = 0, ...} : Set) = set
    | check (set as {size, dead, tree}) =
      if size >= dead * 2 then set
      else let val (size, tree) = batchedRebuild tree
           in {size = size, dead = 0, tree = tree} end

  fun deleteTree (x, E) = E
    | deleteTree (x, T {color, elem = elem as (y, live), left, right}) =
      if Element.lt (x, y) then T {color = color, elem = elem, right = right,
                                   left = deleteTree (x, left)}
      else if Element.lt (y, x) then T {color = color, elem = elem, left = left,
                                        right = deleteTree (x, right)}
      else if live then T {color = color, elem = (y, false),
                           left = left, right = right}
      else raise Fail "Already deleted" (* 削除済みなら新たなツリーを作らない *)
  fun delete (x, {size, dead, tree}) =
      let val tree = deleteTree (x, tree) handle Fail mess => tree
      in check {size = size, dead = dead + 1, tree = tree} end
end
