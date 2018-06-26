local
  open SMLUnit SMLUnit.Assert
  open SegmentedRedundantBinaryNumber

  (* 末尾再帰にしていないので、スタックオーバーフローの可能性あり
   * そんな長い数を使うことになったら修正する *)
  fun splitRed nil = NONE
    | splitRed (ZERO :: ns) = SOME (nil, ns)
    | splitRed (FOUR :: ns) = SOME (nil, ns)
    | splitRed (n :: ns) =
      let
        val opt = splitRed ns
      in
        case opt of
            NONE => NONE
          | SOME (first, rest) => SOME (n :: first, rest)
      end
  fun findGreen nil = false
    | findGreen (TWO :: _) = true
    | findGreen (_ :: ns) = findGreen ns
  (* 普遍条件のテスト関数
   * 簡易的なテストで、Redな数字の前にGreenな数字が必ずある、
   * というテストにしている *)
  fun valid nil = true
    | valid n =
      let
        val opt = splitRed n
      in
        case opt of
            NONE => true
          | SOME (first, rest) => findGreen first andalso valid rest
      end

  fun repeat 0 f x = x
    | repeat n f x = repeat (n - 1) f (f x)
  fun incs n nat = repeat n inc nat
  fun decs n nat = repeat n dec nat
  val hund = incs 100 zero

  val test1 = ("zero invariant", fn () => assertTrue (valid zero))
  val test2 = ("inc invariant1", fn () => assertTrue (valid (incs 10 zero)))
  val test3 = ("inc invariant2", fn () => assertTrue (valid (incs 50 zero)))
  val test4 = ("inc invariant3", fn () => assertTrue (valid (incs 100 zero)))
  val test5 = ("dec invariant1", fn () => assertTrue (valid (decs 10 hund)))
  val test6 = ("dec invariant2", fn () => assertTrue (valid (decs 50 hund)))
  val test7 = ("dec invariant3", fn () => assertTrue (valid (decs 100 hund)))

  val tests =
      Test.TestList
        [
          Test.Test test1,
          Test.Test test2,
          Test.Test test3,
          Test.Test test4,
          Test.Test test5,
          Test.Test test6,
          Test.Test test7,
          Test.TestList nil
        ]
  val tests =
      Test.TestList
        [
          Test.TestLabel ("ex9.12: Segmentedredundantbinarynumber", tests)
        ]
in
structure Ex9_12 = struct val tests = tests end
end
