証明

前者の`drop`と後者の`drop`の等価を示す。

これを、`drop`の引数の構造による帰納法で示す。
つまり、`drop`の定義のパターンマッチで場合分けして示す。

今まででてきた定義より、以下の置き換えを行って同じ式を得られるか確認する。

1. 2種類の`fun lazy drop`をそれぞれ`lazy`アノテーション無しに置き換えしてみる。
2. また、`drop'`が出てきた場合、その定義でさらに置き換える。
3. 仮定より`force ($e)`を`e`へ置き換える。

# 引数が`(0, s)`の場合

## 前者のdrop

```
fun lazy drop (0, s) = s
fun drop x = $case x of (0, s) => force s
```

## 後者のdrop'

```
fun lazy drop (0, s) = drop' (0, s)
```

# パターンマッチ2行目

## 前者のdrop

```
fun lazy drop (n, $NIL) = $NIL
fun drop x = $case x of (n, $NIL) => force ($NIL)
fun drop x = $case x of (n, $NIL) => NIL
```

## 後者のdrop

```
fun lazy drop (n, $NIL) = drop' (n, $NIL)
fun drop x = $case x of (n, $NIL) => force (drop' (n, $NIL))
fun drop x = $case x of (n, $NIL) => force ($NIL)
fun drop x = $case x of (n, $NIL) => NIL
```

# パターンマッチ3行目

## 前者のdrop

```
fun lazy drop (n, $CONS (x, s)) = drop (n-1, s)
```

## 後者のdrop

```
fun lazy drop (n, $CONS (x, s)) = drop' (n, $CONS (x, s))
fun drop x = $case x of (n, $CONS (x, s)) => force (drop' (n, $CONS (x, s)))
fun drop x = $case x of (n, $CONS (x, s)) => force (drop' (n-1, s))
fun lazy drop (n, $CONS (x, s)) = drop' (n-1, s)
```

帰納法の仮定より、`fun lazy drop (n-1, s)`は前者・後者で等しいので、等しい。
（やや怪しい）

よって、すべてのパターンマッチで等価である。
