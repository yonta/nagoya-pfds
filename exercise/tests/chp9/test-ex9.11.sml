local
  open SMLUnit SMLUnit.Assert
  structure Element : ORDERED =
  struct
    type T = int
    fun eq (x : int, y) = x = y
    fun lt (x, y) = x < y
    fun leq (x, y) = x <= y
  end
  structure BH = BinomialHeap (Element)
  open BH

  fun ins nil h = h
    | ins (x :: xs) h = ins xs (insert (x, h))
  fun heap n = ins (List.tabulate (n, fn x => x)) empty
  fun repeat 0 f x = x
    | repeat n f x = repeat (n - 1) f (f x)

  val test1 = ("isEmpty", fn () => assertTrue (isEmpty empty))
  val test2 = ("insert invariant", fn () => assertTrue (valid (heap 100)))
  val test3 =
      ("merge invariant",
       fn () => assertTrue (valid (merge (heap 50, heap 150))))
  val test4 = ("deleteMin invariant",
               fn () => assertTrue (valid (repeat 50 deleteMin (heap 100))))
  val test5 =
      ("deleteMin and findMin",
       fn () =>
          assertEqualInt (100 - 50) (findMin (repeat 50 deleteMin (heap 100))))
  val test6 =
      ("merge and deleteMin and findMin",
       fn () =>
          assertEqualInt
            25
            (findMin (repeat 50 deleteMin (merge (heap 50, heap 100)))))
  val testValid1 =
      ("valid empty", fn () => assertTrue (valid empty))
  val testValid2 =
      ("valid true", fn () => assertTrue (valid (validHeap 0)))
  val testValid3 = ("valid false", fn () => assertFalse (valid (invalidHeap 0)))
  val tests =
      Test.TestList
        [
          Test.Test test1,
          Test.Test test2,
          Test.Test test3,
          Test.Test test4,
          Test.Test test5,
          Test.Test test6,
          Test.TestList (map Test.Test [testValid1, testValid2, testValid3]),
          Test.TestList nil
        ]
  val tests =
      Test.TestList
        [
          Test.TestLabel ("ex9.11: BinomialHeap", tests)
        ]
in
structure Ex9_11 = struct val tests = tests end
end
