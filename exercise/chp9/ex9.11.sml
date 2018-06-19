(* Use "heap.sig", before using this file *)

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

  (* Heap * Heap -> Heap *)
  fun simpleMerge (h1, []) = h1 (* end pattern *)
    | simpleMerge ([], h2) = h2
    | simpleMerge (ONE nil :: _, _) = (* 1-0, 1-1 or 1-2 pattern *)
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
    (* 0-0 or 0-2 pattern *)
    | simpleMerge (ZERO :: ds1, d2 :: ds2) = d2 :: simpleMerge (ds1, ds2)
    (* 2-2 pattern *)
    | simpleMerge ((d1 as TWO tp1) :: ds1, TWO tp2 :: ds2) =
      d1 :: insTree (link tp2, simpleMerge (ds1, ds2))
    | simpleMerge (ds1, ds2) = simpleMerge (ds2, ds1)
    (* HACK: decreese number of patarn match by swaping ds1 and ds2 *)

  (* Heap -> Heap *)
  fun fixup2 nil = nil
    | fixup2 (TWO tp :: ds) = ZERO :: fixup2 (insTree (link tp, ds))
    | fixup2 (d :: ds) = d :: fixup ds

  fun merge h2 = fixup2 (simpleMerge h2)

  (* Tree list -> Tree * Heap *)
  fun removeMinTreeFromOne nil = raise Empty
    | removeMinTreeFromOne [t] = (t, nil)
    | removeMinTreeFromOne (t :: ts) =
      let
        val (t', ds') = removeMinTreeFromOne ts
      in
        if Elem.leq (root t, root t')
        then (t, [ZERO, ONE ts])
        else (t', ones ([t], ds'))
      end

  fun removeHeadZero nil = nil
    | removeHeadZero (ZERO :: ds) = removeHeadZero ds
    | removeHeadZero (ds as _ :: _) = ds
  fun removeEndZero l = (rev o removeHeadZero o rev) l

  (* Heap -> Tree * Heap *)
  fun removeMinTree nil = raise Empty
    | removeMinTree [ZERO] =
      raise Fail "Bug: removeMinTree: argument digit ends with ZERO."
    | removeMinTree [ONE ts] = removeMinTreeFromOne ts
    | removeMinTree [TWO (t1, t2)] =
      let
        val (tsmall, tbig) =
            if Elem.leq (root t1, root t2) then (t1, t2) else (t2, t1)
      in
        (tsmall, [ONE [tbig]])
      end
    | removeMinTree (ZERO :: ds) =
      let val (t', ds') = removeMinTree ds in (t', ZERO :: ds') end
    | removeMinTree (ONE ts :: ds) =
      let
        val (t1, ds1) = removeMinTreeFromOne ts
        val (t2, ds2) = removeMinTree ds
      in
        if Elem.leq (root t1, root t2) then (t1, ds1 @ ds)
        else (t2, ONE ts :: ds2)
      end
    | removeMinTree ((d as TWO (t1, t2)) :: ds) =
      let
        val (tsmall, tbig) = if Elem.leq (root t1, root t2)
                             then (t1, t2) else (t2, t1)
        val (t', ds') = removeMinTree ds
      in
        if Elem.leq (root tsmall, root t')
        then (tsmall, ones ([tbig], ds))
        else (t', d :: ds')
      end

  fun findMin ts = let val (t, _) = removeMinTree ts in root t end
  fun deleteMin ts =
      let
        val (NODE (x, ts1), ts2) = removeMinTree ts
        val ts1' = case ts1 of nil => nil | _::_ => [ONE (rev ts1)]
      in merge (ts1', ts2) end

  fun existsZero ds =
      List.exists (fn d => case d of ZERO => true | _ => false) ds
  fun findTwo nil l = NONE
    | findTwo (TWO _ :: ds) l = SOME (l, ds)
    | findTwo (d :: ds) l = findTwo ds (d :: l)
  fun valid ds =
      let
        val toTwo = findTwo ds nil
      in
        case toTwo of NONE => true
                    | SOME (ds', rest) => existsZero ds' andalso valid rest
      end
end
