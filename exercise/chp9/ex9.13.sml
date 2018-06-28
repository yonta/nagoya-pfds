(* SegmentedRedundantBinaryRandomAccessList *)
structure SRBRandomAccessList : RANDOM_ACCESS_LIST =
struct

  datatype 'a Tree = LEAF of 'a | NODE of 'a Tree * 'a Tree
  datatype 'a Digit = ZERO
                    | ONES of 'a Tree list
                    | TWO of 'a Tree * 'a Tree
                    | THREES of ('a Tree * 'a Tree * 'a Tree) list
                    | FOUR of 'a Tree * 'a Tree * 'a Tree * 'a Tree
  type 'a RList = 'a Digit list

  val empty = nil
  fun isEmpty ds = null ds

  (* 'a Tree * 'a Tree -> 'a Tree *)
  fun link tp = NODE tp
  fun unlink (LEAF _) = raise Fail "Bug: unlink: called with LEAF"
    | unlink (NODE tp) = tp

  fun ones (nil, ds) = ds
    | ones (ts1, ONES ts2 :: ds) = ONES (ts1 @ ts2) :: ds
    | ones (ts1, ds) = ONES ts1 :: ds

  fun threes (nil, ds) = ds
    | threes (tts1, THREES tts2 :: ds) = THREES (tts1 @ tts2) :: ds
    | threes (tts1, ds) = THREES tts1 :: ds

  fun consTree (t, nil) = [ONES [t]]
    | consTree (t, ZERO :: ts) = ones ([t], ts)
    | consTree (_, ONES nil :: _) =
      raise Fail "Bug: consTree: called with ONES nil"
    | consTree (t1, ONES (t2 :: ts2) :: ds) = TWO (t1, t2) :: ones (ts2, ds)
    | consTree (t, TWO (t1, t2) :: ds) = threes ([(t, t1, t2)], ds)
    | consTree (_, THREES nil :: _) =
      raise Fail "Bug: cosTree: called with THREES nil"
    | consTree (t, THREES ((t1, t2, t3) :: tts) :: ds) =
      FOUR (t, t1, t2, t3) :: threes (tts, ds)
    | consTree (_, FOUR _ :: _) = raise Fail "Bug: consTree: called with FOUR"

  fun unconsTree nil = raise Empty
    (* fixupで先頭がZEROのパターンは存在しないのでバグ *)
    | unconsTree (ZERO :: ds) = raise Fail "Bug: unconsTree: called with ZERO"
    | unconsTree [ONES [t]] = (t, empty)
    | unconsTree (ONES (tp :: tps) :: ds) = (tp, ZERO :: ones (tps, ds))
    | unconsTree (ONES nil :: _) =
      raise Fail "Bug: uncosTree: called with ONES nil"
    | unconsTree (TWO (t1, t2) :: ds) = (t1, ones ([t2], ds))
    | unconsTree (THREES nil :: _) =
      raise Fail "Bug: uncosTree: called with THREES nil"
    | unconsTree (THREES ((t1, t2, t3) :: tts) :: ds) =
      (t1, TWO (t2, t3) :: threes (tts, ds))
    | unconsTree (FOUR (t1, t2, t3, t4) :: ds) =
      (t1, threes ([(t2, t3, t4)], ds))

  (* fixupは、このデータの普遍条件である(G|Y|GY*R)*のような列を維持する。
   * 実装はex9.12.smlのfixupを参考にしている。 *)
  fun fixup nil = nil
    | fixup [ZERO] = nil
    | fixup (ZERO :: ds) =
      let val (t, ds') = unconsTree ds
      in  TWO (unlink t) :: ds' end
    | fixup (ONES ts :: ZERO :: ds) =
      let val (t, ds') = unconsTree ds
      in  ONES ts :: TWO (unlink t) :: ds' end
    | fixup (THREES tts :: FOUR (t1, t2, t3, t4) :: ds) =
      THREES tts :: TWO (t1, t2) :: consTree (link (t3, t4), ds)
    | fixup (FOUR (t1, t2, t3, t4) :: ds) =
      TWO (t1, t2) :: consTree (link (t3, t4), ds)
    | fixup ds = ds

  fun cons (x, ds) = fixup (consTree (LEAF x, ds))

  fun head nil = raise Empty
    | head ds =
      let
        val (t, _) = unconsTree ds
      in
        case t of
            LEAF x => x
          | NODE _ => raise Fail "Bug: head: unconsTree made NODE object"
      end
  fun tail nil = raise Empty
    | tail ds =
      let val (_, ds') = unconsTree ds
      in fixup ds' end

  fun lookupTreeN (0, LEAF x, _) = x
    | lookupTreeN (_, LEAF _, _) = raise Subscript
    | lookupTreeN (i, NODE (t1, t2), n) =
      if i < n div 2 then lookupTreeN (i, t1, n div 2)
      else lookupTreeN (i - n div 2, t2, n div 2)

  fun lookupn (_, nil, _) = raise Subscript
    | lookupn (i, ZERO :: ds, n) = lookupn (i, ds, n * 2)
    | lookupn (_, ONES nil :: _, _) =
      raise Fail "Bug: lookupn: called with ONE nil"
    | lookupn (i, ONES (t :: ts) :: ds, n) =
      if i < n then lookupTreeN (i, t, n)
      else let val next = case ts of nil => ds | _ => ONES ts :: ds
           in  lookupn (i - n, next, n * 2) end
    | lookupn (i, TWO (t1, t2) :: ds, n) =
      if i < n then lookupTreeN (i, t1, n)
      else if i < 2 * n then lookupTreeN (i - n, t2, n)
      else lookupn (i - 2 * n, ds, n * 2)
    | lookupn (_, THREES nil :: _, _) =
      raise Fail "Bug: lookupn: called with THREES nil"
    | lookupn (i, THREES ((t1, t2, t3) :: tts) :: ds, n) =
      if i < n then lookupTreeN (i, t1, n)
      else if i < 2 * n then lookupTreeN (i - n, t2, n)
      else if i < 3 * n then lookupTreeN (i - 2 * n, t3, n)
      else let val next = case tts of nil => ds | _ => THREES tts :: ds
           in  lookupn (i - 3 * n, next, n * 2) end
    | lookupn (i, FOUR (t1, t2, t3, t4) :: ds, n) =
      if i < n then lookupTreeN (i, t1, n)
      else if i < 2 * n then lookupTreeN (i - n, t2, n)
      else if i < 3 * n then lookupTreeN (i - 2 * n, t3, n)
      else if i < 4 * n then lookupTreeN (i - 3 * n, t4, n)
      else lookupn (i - 4 * n, ds, n * 2)

  fun lookup (i, ds) = lookupn (i, ds, 1)

  fun update _ = raise Fail "not impled"
end
