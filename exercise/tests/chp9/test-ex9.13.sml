local
  open SMLUnit SMLUnit.Assert
  open SRBRandomAccessList

  fun repeat 0 f x = x
    | repeat n f x = repeat (n - 1) f (f x)
  fun consn 0 ds = ds
    | consn n ds = consn (n - 1) (cons (n, ds))
  fun tailn n ds = repeat n tail ds

  val test1 = ("cons", fn () => assertTrue (consn 100 empty; true))
  val test2 = ("head tail1",
               fn () => assertEqualInt 1 (head (consn 100 empty)))
  val test2 =
      ("head tail2",
       fn () => assertEqualInt 51 (head (tailn 50 (consn 100 empty))))
  val test3 =
      ("head tail3",
       fn () => assertEqualInt 100 (head (tailn 99 (consn 100 empty))))
  val test4 =
      ("lookup1", fn () => assertEqualInt 1 (lookup (0, consn 100 empty)))
  val test5 =
      ("lookup2", fn () => assertEqualInt 50 (lookup (49, consn 100 empty)))
  val test6 =
      ("lookup3", fn () => assertEqualInt 100 (lookup (99, consn 100 empty)))

  val tests =
      Test.TestList
        [
          Test.Test test1,
          Test.Test test2,
          Test.Test test3,
          Test.Test test4,
          Test.Test test5,
          Test.Test test6,
          Test.TestList nil
        ]
  val tests =
      Test.TestList
        [
          Test.TestLabel ("ex9.13: SRBRandomAccessList", tests)
        ]
in
structure Ex9_13 = struct val tests = tests end
end
