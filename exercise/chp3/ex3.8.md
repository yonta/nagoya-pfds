赤黒木Tのサイズをsize(T)、最大深さをmaxDepth(T)と表すことにする。

すると証明したい定理は以下のように書ける。

```
size(T) = n -> maxDepth(T) <= 2 floor(log(n+1))
```

これを赤黒木Tの構造に関する帰納法でしめす。

# T = Eの場合

maxDepth(T) = 0
2 floor(log(0+1)) = 0

で明らか

# T = Eの場合

帰納法の仮定より、Tよりサイズが１小さいT'で定理が成り立つ。
よって、

maxDepth(T') <= 2 floor(log(n))

このとき、証明したい定理の左辺と右辺は以下の場合がありうる

maxDepth(T) = maxDepth(T')
            or maxDepth(T') + 1     ---1

2 floor(log(n+1)) = 2 floor(log(n))         ---2
                  or 2 floor(log(n)) + 2

1かつ2のパターンがやばいが、証明する画期的な発想が足りなかったので、
またあとで考える。
