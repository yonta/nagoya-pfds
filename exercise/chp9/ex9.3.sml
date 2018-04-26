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
  type 'a RList = 'a Tree list

  val empty = nil
  fun isEmpty ts = null ts

  fun size (LEAF _) = 1
    | size (NODE (w, _, _)) = w

  fun link (t1, t2) = NODE (size t1 + size t2, t1, t2)
  fun consTree (t, nil) = [t]
    | consTree (t1, ts as t2 :: ts2) =
      if size t1 = size t2 then consTree (link (t1, t2), ts) else t1 :: ts
  fun cons (x, ts) = consTree (LEAF x, ts)

  fun unconsTree (t as LEAF _, accum) = (t, accum)
    | unconsTree (NODE (_, t1, t2), accum) = unconsTree (t1, t2 :: accum)
  fun unconsRList nil = raise Empty
    | unconsRList ((t as LEAF _) :: ts) = (t, ts)
    | unconsRList ((t as NODE _) :: ts) = unconsTree (t, ts)
  fun head ts =
      case unconsRList ts of
          (LEAF x, _) => x
        | (NODE _, _) => raise Fail "Bug: head: unconsRList returns NODE."
  fun tail ts = let val (_, ts') = unconsRList ts in ts' end

  fun lookupTree (0, LEAF x) = x
    | lookupTree (i, LEAF _) = raise Subscript
    | lookupTree (i, NODE (w, t1, t2)) =
      if i < w div 2 then lookupTree (i, t1)
      else lookupTree (i - w div 2, t2)
  fun lookup (i, nil) = raise Subscript
    | lookup (i, t :: ts) =
      if i < size t then lookupTree (i, t) else lookup (i - size t, ts)

  fun updateTree (0, y, LEAF _) = LEAF y
    | updateTree (i, _, LEAF _) = raise Subscript
    | updateTree (i, y, NODE (w, t1, t2)) =
      if i < w div 2 then NODE (w, updateTree (i, y, t1), t2)
      else NODE (w, t1, updateTree (i - w div 2, y, t2))
  fun update (_, _, nil) = raise Subscript
    | update (i, y, t :: ts) =
      if i < size t then (updateTree (i, y, t)) :: ts
      else t :: update (i - size t, y, ts)
end
