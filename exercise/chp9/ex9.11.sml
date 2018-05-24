signature ORDERED =
sig
  type T
  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature HEAP =
sig
  structure Elem : ORDERED
  type Heap
  val empty     : Heap
  val isEmpty   : Heap -> bool
  val insert    : Elem.T * Heap -> Heap
  val merge     : Heap * Heap -> Heap
  val findMin   : Heap -> Elem.T
  val deleteMin : Heap -> Heap
end

(*
structure Element : ORDERED =
struct
  type T = int
  fun eq (x : int, y) = x = y
  fun lt (x, y) = x < y
  fun leq (x, y) = x <= y
end
*)

functor BinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = NODE of Elem.T * Tree list
  datatype Digit = ZERO | ONE of Tree list | TWO of Tree * Tree
  type Heap = Digit list

  val empty = nil
  fun isEmpty ts = null ts

  fun root (NODE (x, ts)) = x

  (* Tree list * Digit list -> Digit list *)
  fun ones (nil, ds) = ds
    | ones (ts, ONE ts1 :: ds) = ONE (ts @ ts1) :: ds
    | ones (ts, ds) = ONE ts :: ds

  (* fun zeros _ = raise Fail "" *)


  (* Tree * Heap -> Heap *)
  fun simpleInsTree (t, nil) = [ONE [t]]
    | simpleInsTree (t, ZERO :: ds) = ones ([t], ds)
    | simpleInsTree (_, ONE nil :: _) =
      raise Fail "Bug: SimpleInsTree: ONE with nil"
    | simpleInsTree (t, ONE (t1 :: ts1) :: ds) = TWO (t, t1) :: ones (ts1, ds)
    | simpleInsTree (_, TWO _ :: _) =
      raise Fail "Bug: simpleInsTree: start with TWO"

  (* Tree * Tree -> Tree *)
  (* Always, size t1 and size t2 are same. *)
  fun link (t1 as NODE (x1, c1), t2 as NODE (x2, c2)) =
      if Elem.leq (x1, x2) then NODE (x1, t2 :: c1)
      else NODE (x2, t1 :: c2)

  (* Heap -> Heap *)
  fun fixup (TWO tp :: ds) = ZERO :: simpleInsTree (link tp, ds)
    | fixup ((one as ONE ts) :: TWO tp :: ds) =
      one :: ZERO :: simpleInsTree (link tp, ds)
    | fixup ds = ds

  (* Tree * Heap -> Heap *)
  fun insTree th = fixup (simpleInsTree th)

  fun insert (x, h) = insTree (NODE (x, []), h)

  fun simpleMerge (h1, []) = h1
    | simpleMerge ([], h2) = h2
    | simpleMerge (ONE nil :: _, _) =
      raise Fail "Bug: simpleMerge: one with nil"
    | simpleMerge (ONE (t1 :: ts1) :: ds1, ZERO :: ds2) =
      (* both pattarn of ONE arg with [t1] and [t1 :: ts1(not nil)] *)
      let val ds1 = case ts1 of nil => ds1 | _ :: _ => ONE ts1 :: ds1
      in  ones ([t1], simpleMerge (ds1, ds2)) end
    | simpleMerge (ONE (t1 :: ts1) :: ds1, ONE (t2 :: ts2) :: ds2) =
      let (* both pattarn of ONE arg with [t1] and [t1 :: ts1(not nil)] *)
        val ds1 = case ts1 of nil => ds1 | _ :: _ => ONE ts1 :: ds1
        val ds2 = case ts2 of nil => ds2 | _ :: _ => ONE ts2 :: ds2
      in
        TWO (t1, t2) :: simpleMerge (ds1, ds2)
      end
    | simpleMerge (ONE (t1 :: ts1) :: ds1, TWO tp2 :: ds2) =
      (* both pattarn of ONE arg with [t1] and [t1 :: ts1(not nil)] *)
      let val ds1 = case ts1 of nil => ds1 | _ :: _ => ONE ts1 :: ds1
      in  ones ([t1], insTree (link tp2 ,simpleMerge (ds1, ds2))) end
    | simpleMerge (ds1, ds2 as ONE _ :: _) = simpleMerge (ds2, ds1)
    (* HACK: decreese number of patarn match by swaping ds1 and ds2 *)
    | simpleMerge (ZERO :: ds1, d2 :: ds2) =
      d2 :: simpleMerge (ds1, ds2)
    | simpleMerge (d1 :: ds1, ZERO :: ds2) =
      d1 :: simpleMerge (ds1, ds2)
    | simpleMerge ((d1 as TWO tp1) :: ds1, TWO tp2 :: ds2) =
      d1 :: insTree (link tp2, simpleMerge (ds1, ds2))

  fun fixup2 _ = raise Fail "Unimplemented"

  fun merge h2 = fixup2 (simpleMerge h2)

  fun findMin _ = raise Fail "Unimplemented"
  fun deleteMin _ = raise Fail "Unimplemented"
(*
  fun removeMinTree [] = raise Empty
    | removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
        let val (t', ts') = removeMinTree ts
        in if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts') end

  fun findMin ts = let val (t, _) = removeMinTree ts in root t end
  fun deleteMin ts =
        let val (Node (_, x, ts1), ts2) = removeMinTree ts
        in merge (rev ts1, ts2) end
*)
end
