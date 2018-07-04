signature RANDOM_ACCESS_LIST =
sig
  type 'a RList

  val empty   : 'a RList
  val isEmpty : 'a RList -> bool

  val cons    : 'a * 'a RList -> 'a RList
  (* head and tail raise Empty if list is empty *)
  val head    : 'a RList -> 'a
  val tail    : 'a RList -> 'a RList

  (* lookup and update raise Subscript if index is out of bounds *)
  val lookup  : int * 'a RList -> 'a
  val update  : int * 'a * 'a RList -> 'a RList
end
