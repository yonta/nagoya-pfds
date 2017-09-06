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

  val findMin   : Heap -> Elem.T   (* raises Empty if heap is empty *)
  val deleteMin : Heap -> Heap     (* raises Empty if heap is empty *)
end

functor PairingHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  (* datatype Heap = E | T of Elem.T * Heap list *)
  (* トップの右は常にEが入っている *)
  datatype Heap = E | T of Elem.T * Heap * Heap

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (h, E) = h
    | merge (E, h) = h
    | merge (h1 as T (x, left1, E), h2 as T (y, left2, E)) =
      if Elem.leq (x, y) then T (x, T (y, left2, left1), E)
      else T (y, T (x, left1, left2), E)
  fun insert (x, h) = merge (T (x, E, E), h)

  fun mergePairs E = E
    | mergePairs (heap as T (_, _, E)) = heap
    | mergePairs (T (x, left1, T (y, left2, right2))) =
      merge (merge (T (x, left1, E), T (y, left2, E)), mergePairs right2)

  fun findMin E = raise Empty
    | findMin (T (x, _, _)) = x
  fun deleteMin E = raise Empty
    | deleteMin (T (x, left, E)) = mergePairs left
end

local
  structure Elem : ORDERED =
  struct
    open Int
    type T = int
    fun eq (x : T, y) = x = y     (* delete warning of polyEqual *)
    fun lt (x, y) = x < y
    fun leq (x, y) = x <= y
  end
  structure PH = PairingHeap (Elem)
  open PH
  fun fromList nil = empty
    | fromList (x::xs) = insert (x, fromList xs)
in
val h1 = fromList [1, 2, 3, 4, 5, 6, 7, 8]
val h2 = deleteMin h1
val h3 = fromList [8, 7, 6, 5, 4, 3, 2, 1]
val h4 = deleteMin h3
end
