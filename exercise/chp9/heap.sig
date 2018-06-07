signature ORDERED =
sig
  type T
  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature HEAP =
sig
  structure Elem : ORDERED
  type Heap
  val empty     : Heap
  val isEmpty   : Heap -> bool
  val insert    : Elem.T * Heap -> Heap
  val merge     : Heap * Heap -> Heap
  val findMin   : Heap -> Elem.T
  val deleteMin : Heap -> Heap

  val valid : Heap -> bool      (* for unit test *)
end
