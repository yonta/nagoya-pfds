datatype Heap = E | T of int * Heap list
datatype BinTree = E' | T' of int * BinTree * BinTree

local
  fun toBinaryImpl E nil = E'
    | toBinaryImpl E (h::hs) = raise Empty (* Pairing Heapの定義よりEはない *)
    | toBinaryImpl (T (x, nil)) nil = T' (x, E', E')
    | toBinaryImpl (T (x, nil)) (h::hs) = T' (x, E', toBinaryImpl h hs)
    | toBinaryImpl (T (x, h::hs)) nil = T' (x, toBinaryImpl h hs, E')
    | toBinaryImpl (T (x, h1::hs1)) (h2::hs2) =
      T' (x, toBinaryImpl h1 hs1, toBinaryImpl h2 hs2)
in
fun toBinary heap = toBinaryImpl heap nil
end

(*
 *         1
 *      /  |   \
 *    2    3     4
 *   / \   |   / | \
 *  5   6  7  8  9  10
 *)
val heap = T (1,
              [
                T (2, [T (5, nil), T (6, nil)]),
                T (3, [T (7, nil)]),
                T (4, [T (8, nil), T (9, nil), T (10, nil)])
              ]
             )
(*
 *        1
 *       /
 *      2
 *    /   \
 *   5     3
 *    \   / \
 *     6 7   4
 *          /
 *         8
 *          \
 *           9
 *            \
 *             10
 *)
val btree = toBinary heap
