# 課題3.1

size T : ヒープTの要素数
rank T : ヒープTの右スパイン
とすると、
n = size Tのときrank T <= floor( log(n+1) )を示す。

## 証明

左偏ヒープの構造に関する帰納法で示す。

### 左偏ヒープの深さが1のとき

対象のヒープ（T）の要素数n（size T）より小さいことを証明するために、
最悪条件である最小の要素数nのパターンを証明する。
つまり、証明するヒープの形状は下記である

```
  x
 / \
E   E
```

このとき、このヒープの要素数は1、ヒープの右スパインは1である。
よって、
1 = rank T <= floor ( log(n+1) ) = 1
となり証明できた。

### 左偏ヒープの深さがkのとき

帰納法の仮定より、サブヒープでは証明しようとしている結論が成り立つ。
よって、対象のヒープTの右サブヒープをR、左サブヒープをLとすると、

rank L <= floor( log(m+1) )
rank R <= floor( log(n+1) )

ただし、

m = size L
n = size R

このとき、ヒープの定義より、

rank L >= rank R

よって、

floor( log(m+1) ) >= floor( log(n+1) )

つまり下記の式が得られる。

m >= n

ここで、結論の左辺である、ヒープTに対するrank Tを考える。

rank T
= rank R + 1
= floor( log(n+1) ) + 1

次に、結論の右辺である式を考える。

floor( log(size T + 1) )
= floor( log((m+n+1) + 1) )

m >= n より

.>= floor( log((n+n+1) + 1) )
= floor( log(2n+2) )
= floor( log2 + log(n+1) )
= floor( log(n+1) + 1 )

よって、
rank T <= floor( log(size T + 1) )
