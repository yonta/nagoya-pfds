```sml
fun deleteMin E = raise Empty
  | deleteMin (T (E, x, b)) = b
  | deleteMin (T (T (E, x, b), y, c)) = T (b, y, c)
  | deleteMin (T (T (a, x, b), y, c)) = T (deleteMin a, x, T (b, y, c))
```

スプレイヒープtに対するdeleteMinの償却コストをB(t)として、求める。
