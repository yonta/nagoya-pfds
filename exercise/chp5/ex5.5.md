# ex 5.5 zig-zagケースの証明

  s = x        t' =  y    s' = x
     / \            / \       / \
t = y   d   =>     c   a     b   d
   / \
  c   u

ただし、`val (a, b) = partition (pivot, u)`

  A(s)

  A(s)の定義より

= T(s) + Φ(t') + Φ(s') - Φ(s)

  T(s) = 1 + T(u)より（ただし、この1は何を言っているのかがテキストでは曖昧）

= 1 + T(u) + Φ(t') + Φ(s') - Φ(s)

  Tの定義より
    T(u) = A(u) - Φ(a) - Φ(b) + Φ(u)

= 1 + A(u) - Φ(a) - Φ(b) + Φ(u) + Φ(t') + Φ(s') - Φ(s)

  Φの定義より、
    Φ(t') = φ(y) + Φ(c) + Φ(a)
    Φ(s') = φ(x) + Φ(b) + Φ(d)
    Φ(t) = φ(y) + Φ(c) + Φ(u)
    Φ(s) = φ(s) + Φ(t) + Φ(d)
         = φ(s) + φ(y) + Φ(c) + Φ(d) + Φ(u)

= 1 + A(u) - Φ(a) - Φ(b) + Φ(u)
  + {φ(y) + Φ(c) + Φ(a)} + {φ(x) + Φ(b) + Φ(d)}
  - {φ(s) + φ(y) + Φ(c) + Φ(d) + Φ(u)}

  項の和と差を整理

= 1 + A(u) + φ(y) + φ(x) - φ(s) - φ(y)

  機能法の仮定より
    A(u) <= 1 + 2φ(u)

<= 1 + 1 + 2φ(u) + φ(y) + φ(x) - φ(s) - φ(y)

  φ(u) <= φ(s), φ(u) <= φ(y)より、

<= 1 + 1 + φ(y) + φ(x)

  #y + #x = #sなので、補題5.1より

<= 1 + 2φ(s)

QED.
