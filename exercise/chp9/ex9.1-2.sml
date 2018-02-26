signature RANDOM_ACCESS_LIST =
sig
  type 'a RList

  val empty   : 'a RList
  val isEmpty : 'a RList -> bool

  val cons    : 'a * 'a RList -> 'a RList
  val head    : 'a RList -> 'a
  val tail    : 'a RList -> 'a RList

  val lookup  : int * 'a RList -> 'a
  val update  : int * 'a * 'a RList -> 'a RList

  val drop : int * 'a RList -> 'a RList (* ex9.1 *)
  val create : int * 'a -> 'a RList     (* ex9.2 *)
end

structure BinaryRandomAccessList : RANDOM_ACCESS_LIST =
struct
  datatype 'a Tree = Leaf of 'a | Node of int * 'a Tree * 'a Tree
  datatype 'a Digit = Zero | One of 'a Tree
  type 'a RList = 'a Digit list

  val empty = []
  fun isEmpty ts = null ts

  fun size (Leaf x) = 1
    | size (Node (w, t1, t2)) = w
  fun link (t1, t2) = Node (size t1+size t2, t1, t2)
  fun consTree (t, []) = [One t]
    | consTree (t, Zero :: ts) = One t :: ts
    | consTree (t1, One t2 :: ts) = Zero :: consTree (link (t1, t2), ts)
  fun unconsTree [] = raise Empty
    | unconsTree [One t] = (t, [])
    | unconsTree (One t :: ts) = (t, Zero :: ts)
    | unconsTree (Zero :: ts) =
        let
          val (t, ts') = unconsTree ts
        in
          case t of
              Node (_, t1, t2) => (t1, One t2 :: ts')
            | Leaf _ => raise Fail "Bug: unconsTree: Leaf pattern"
        end

  fun cons (x, ts) = consTree (Leaf x, ts)
  fun head ts =
      let
        val (t, _) = unconsTree ts
      in
        case t of
            Leaf x => x
          | Node _ => raise Fail "Bug: head: Node pattern"
      end
  fun tail ts = let val (_, ts') = unconsTree ts in ts' end

  fun lookupTree (0, Leaf x) = x
    | lookupTree (i, Leaf x) = raise Subscript
    | lookupTree (i, Node (w, t1, t2)) =
        if i < w div 2 then lookupTree (i, t1)
        else lookupTree (i - w div 2, t2)
  fun updateTree (0, y, Leaf x) = Leaf y
    | updateTree (i, y, Leaf x) = raise Subscript
    | updateTree (i, y, Node (w, t1, t2)) =
        if i < w div 2 then Node (w, updateTree (i, y, t1), t2)
        else Node (w, t1, updateTree (i - w div 2, y, t2))

  fun lookup (i, []) = raise Subscript
    | lookup (i, Zero :: ts) = lookup (i, ts)
    | lookup (i, One t :: ts) =
        if i < size t then lookupTree (i, t) else lookup (i - size t, ts)
  fun update (i, y, []) = raise Subscript
    | update (i, y, Zero :: ts) = Zero :: update (i, y, ts)
    | update (i, y, One t :: ts) =
        if i < size t then One (updateTree (i, y, t)) :: ts
        else One t :: update (i - size t, y, ts)

  (* ex9.1 *)
  (* called by "drop(i, t)" with "i <= size t" *)
  fun dropTree (0, t as Leaf _, accum) = One t :: accum
    | dropTree (1, Leaf _, accum) = accum
    | dropTree (_, Leaf _, _) = raise Fail "Bug: dropTree: called by i > size t"
    | dropTree (i, Node (w, t1, t2), accum) =
      if i <= w div 2 then dropTree (i, t1, One t2 :: accum)
      else dropTree (i - w div 2, t2, Zero :: accum)

  (* needs i >= 0 *)
  fun drop (0, t) = t
    | drop (i, nil) = nil       (* not Subscript exception *)
    | drop (i, Zero :: ts)  = drop (i, ts)
    | drop (i, One t :: ts) =
      if i < size t then dropTree (i, t, ts)
      else drop (i - size t, ts)

  (* ex9.2 *)
  fun create (n, x) = raise Fail "unimplemented"
end

local
  open BinaryRandomAccessList
  fun consN 0 l = l
    | consN n l = consN (n-1) (cons (n, l))
in
val l1 = consN 10 empty
val l2 = drop (5, l1)
(*
val l3 = create (10, 0)
val l4 = drop (5, l3)
*)
end
