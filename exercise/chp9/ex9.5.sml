signature RANDOM_ACCESS_LIST =
sig
  type 'a RList

  val empty   : 'a RList
  val isEmpty : 'a RList -> bool

  val cons    : 'a * 'a RList -> 'a RList
  val head    : 'a RList -> 'a
  val tail    : 'a RList -> 'a RList
  (* head and tail raise Empty if list is empty *)

  val lookup  : int * 'a RList -> 'a
  val update  : int * 'a * 'a RList -> 'a RList
  (* lookup and update raise Subscript if index is out of bounds *)
end

structure BinaryRandomAccessList : RANDOM_ACCESS_LIST =
struct
  datatype 'a Tree = LEAF of 'a | NODE of int * 'a Tree * 'a Tree
  datatype 'a Digit = ONE of 'a Tree | TWO of 'a Tree * 'a Tree
  type 'a RList = 'a Digit list

  val empty = nil
  fun isEmpty ts = null ts

  fun size (LEAF _) = 1
    | size (NODE (w, _, _)) = w
  fun link (t1, t2) = NODE (size t1 + size t2, t1, t2)
  fun consTree (t, nil) = [ONE t]
    | consTree (t1, ONE t2 :: ts) = TWO (t1, t2) :: ts
    | consTree (t1, TWO t2 :: ts) = ONE t1 :: consTree (link t2, ts)
  fun cons (x, ts) = consTree (LEAF x, ts)

  fun unconsTree nil = raise Empty
    | unconsTree [ONE t] = (t, nil)
    | unconsTree (ONE t :: ts) =
      (case unconsTree ts of
           (NODE (_, t1, t2), ts') => (t, TWO (t1, t2) :: ts')
         | (LEAF _, _) =>
           raise Fail "Bug: unconsTree: recursive call returns LEAF")
    | unconsTree (TWO (t1, t2) :: ts) = (t1, ONE t2 :: ts)
  fun head ts =
      case unconsTree ts of
          (LEAF x, _) => x
        | (NODE _, _) => raise Fail "Bug: head: unconsTree returns NODE"
  fun tail ts = let val (_, ts') = unconsTree ts in ts' end

  fun lookupTree (0, LEAF x) = x
    | lookupTree (_, LEAF _) = raise Subscript
    | lookupTree (i, NODE (w, t1, t2)) =
      if i < w div 2 then lookupTree (i, t1)
      else lookupTree (i - w div 2, t2)
  fun lookup (i, nil) = raise Subscript
    | lookup (i, ONE t :: ts) =
      if i < size t then lookupTree (i, t) else lookup (i - size t, ts)
    | lookup (i, TWO (t1, t2) :: ts) =
      if i < size t1 then lookupTree (i, t1)
      else if i - size t1 < size t2 then lookupTree (i - size t1, t2)
      else lookup (i - size t1 - size t2, ts)

  fun updateTree (0, y, LEAF x) = LEAF y
    | updateTree (_, _, LEAF _) = raise Subscript
    | updateTree (i, y, NODE (w, t1, t2)) =
      if i < w div 2 then NODE (w, updateTree (i, y, t1), t2)
      else NODE (w, t1, updateTree (i - w div 2, y, t2))
  fun update (_, _, []) = raise Subscript
    | update (i, y, ONE t :: ts) =
      if i < size t then ONE (updateTree (i, y, t)) :: ts
      else ONE t :: update (i - size t, y, ts)
    | update (i, y, (t as TWO (t1, t2)) :: ts) =
      if i < size t1 then TWO (updateTree (i, y, t1), t2) :: ts
      else if i - size t1 < size t2 then TWO (t1, updateTree (i, y, t2)) :: ts
      else t :: update (i - size t1 - size t2, y, ts)

end
