signature QUEUE =
sig
  type 'a Queue
  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool
  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a
  val tail    : 'a Queue -> 'a Queue
end

signature OUTPUT_RESTRICTED_DEQUE =
sig
  include QUEUE
  val cons : 'a * 'a Queue -> 'a Queue
end

functor AddCons (Q : QUEUE) : OUTPUT_RESTRICTED_DEQUE =
struct
  type 'a Queue = 'a list * 'a Q.Queue
  val empty = (nil, Q.empty)
  fun isEmpty (l, q) = null l andalso Q.isEmpty q
  fun snoc ((l, q), x) = (l, Q.snoc (q, x))
  fun head (nil, q) = Q.head q
    | head (x::xs, q) = x
  fun tail (nil, q) = (nil, Q.tail q)
    | tail (x::xs, q) = (xs, q)
  fun cons (x, (l, q)) = (x::l, q)
end
