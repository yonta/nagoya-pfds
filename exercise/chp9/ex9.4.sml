datatype Digit = ONE | TWO
type Nat = Digit list

fun inc nil = [ONE]
  | inc (ONE :: ds) = TWO :: ds
  | inc (TWO :: ds) = ONE :: inc ds

fun dec nil = raise Fail "dec: dec by ZERO"
  | dec (ONE :: ds) = TWO :: dec ds
  | dec (TWO :: ds) = ONE :: ds

fun add nil n2 = n2
  | add n1 nil = n1
  | add (ONE :: n1) (ONE :: n2) = TWO :: add n1 n2
  | add (ONE :: n1) (TWO :: n2) = ONE :: inc (add n1 n2)
  | add (TWO :: n1) (ONE :: n2) = ONE :: inc (add n1 n2)
  | add (TWO :: n1) (TWO :: n2) = TWO :: inc (add n1 n2)
