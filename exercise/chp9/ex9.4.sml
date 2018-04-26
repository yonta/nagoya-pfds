datatype Digit = One | Two
type Nat = Digit list

fun inc nil = [One]
  | inc (One :: ds) = Two :: ds
  | inc (Two :: ds) = One :: inc ds

fun dec nil = raise Fail "dec: dec by ZERO"
  | dec (One :: ds) = Two :: dec ds
  | dec (Two :: ds) = One :: ds

fun add nil n2 = n2
  | add n1 nil = n1
  | add (One :: n1) (One :: n2) = Two :: add n1 n2
  | add (One :: n1) (Two :: n2) = One :: inc (add n1 n2)
  | add (Two :: n1) (One :: n2) = One :: inc (add n1 n2)
  | add (Two :: n1) (Two :: n2) = Two :: inc (add n1 n2)
