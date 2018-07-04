local
  open SMLUnit SMLUnit.Assert
  open HoodMelvilleQueue

  fun repeat 0 f x = x
    | repeat n f x = repeat (n - 1) f (f x)
  fun snocn 0 q = q
    | snocn n q = snocn (n - 1) (snoc (q, n))
  fun tailn n ds = repeat n tail ds

  val test1 = ("isEmpty", fn () => assertTrue (isEmpty empty))
  val test2 = ("snoc and head",
               fn () => assertEqualInt 1 (head (snocn 1 empty)))
  val test3 = ("snoc and head",
               fn () => assertEqualInt 50 (head (snocn 50 empty)))
  val test4 = ("snoc and head",
               fn () => assertEqualInt 100 (head (snocn 100 empty)))
  val test5 =
      ("snoc, tail and head",
       fn () => assertEqualInt 99 (head (tailn 1 (snocn 100 empty))))
  val test6 =
      ("snoc, tail and head",
       fn () => assertEqualInt 50 (head (tailn 50 (snocn 100 empty))))
  val test7 =
      ("snoc, tail and head",
       fn () => assertEqualInt 1 (head (tailn 99 (snocn 100 empty))))

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
          Test.TestLabel
            (
              "ex9.14: HoodMelvilleQueue with SkewBinarryRandomAccessList",
              tests
            )
        ]
in
structure Ex9_14 = struct val tests = tests end
end
