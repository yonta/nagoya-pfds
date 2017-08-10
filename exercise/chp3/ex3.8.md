赤黒木Tのサイズをsize(T)、最大深さをmaxDepth(T)と表すことにする。

すると証明したい定理は以下のように書ける。

```
size(T) = n -> maxDepth(T) <= 2 floor(log(n+1))
```

これを赤黒木Tの構造に関する帰納法でしめす。

# 木TがEの場合

maxDepth(T) = 0
2 floor(log(0+1)) = 0

で明らか

# 木TがT (color, left, x, right)の場合

帰納法の仮定より、部分木left/rightでは結論が成り立つ。
よって、

maxDepth(left)  <= 2 floor(log(size(left)+1))
maxDepth(right) <= 2 floor(log(size(right)+1))

また、ここで部分木は左のほうが深さが大きいと仮定する。
つまり、size(left) >= size(right)である。
ツリーに左右の違いはないため、この仮定は証明に影響を及ぼさない。
すると、ツリーの構造から次が成り立つ。

size(T) = size(left) + size(right) + 1
maxDepth(T) = maxDepth(left) + 1

故に、

maxdepth(T) <= 2 floor(log(size(left)+1)) + 1

size(T) = size(left) + size(right) + 1
size(left) + 1 = size(T) - size(right)
               <= size(T) / 2

よって
maxdepth(T) <= 2 floor(log(size(T)/2)) + 1
             = 2 floor(log(size(T))) -2 + 1
             = 2 floor(log(size(T))) - 1
            <= 2 floor(log(size(T)))

よって証明できた。
