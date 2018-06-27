local
  open SMLUnit SMLUnit.Assert

  val tests =
      Test.TestList
        [
          Ex9_11.tests,
          Ex9_12.tests,
          Ex9_13.tests,
          Test.TestList nil
        ]
  fun main () = TextUITestRunner.runTest {output = TextIO.stdOut} tests
in
val () = main ()
end
