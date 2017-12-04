```sml
type 'a Queue = int * 'a Stream * int * 'a Stream

fun rotate ($Nil, $(Cons (y, _), a) = $(Cons (y, a))
  | rotate ($(Cons (x, xs)), $(Cons (y, ys)), a) =
    $(Cons (x, rotate (xs, ys, $(Cons (y, a)))))

(* old
fun check (q as (lenf, f, lenr, r)) =
  if lenr <= lenf then q else (lenf+lenr, f ++ reverse r, 0, $Nil)
 *)

(* new *)
fun check (q as (lenf, f, lenr, r)) =
  if lenr <= lenf then q else (lenf+lenr, rotate (f, r, $NIL), 0, nil)

fun snoc ((lenf, f, lenr, r), x) = check (lenf, f, lenr+1, $(Cons (x, r)))

fun head (lenf, $Nil, lenr, r) = raise Empty
  | head (lenf, $(Cons (x, f')), lenr, r) = x

fun tail (lenf, $Nil, lenr, r) = raise Empty
  | tail (lenf, $(Cons (x, f')), lenr, r) = check (lenf-1, f', lenr, r)
```
