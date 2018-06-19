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
  val test2 = ("Invariant", fn () => assertTrue (valid (heap 100)))
  val test3 =
      ("deleteMin",
       fn () =>
          assertEqualInt (100 - 50) (findMin (repeat 50 deleteMin (heap 100))))
in
val tests =
    Test.TestList
      [
        Test.Test test1,
        Test.Test test2,
        Test.Test test3,
        Test.TestList nil
      ]
val tests =
    Test.TestList
      [
        Test.TestLabel ("BinomialHeap", tests)
      ]

fun main () = TextUITestRunner.runTest {output = TextIO.stdOut} tests
end

val () = main ()
