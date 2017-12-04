# findMinの償却コストがO(log(n))である証明

これらはremoveMinTreeを使っているため、まず、この関数の償却コストを調べる。

removeMinTreeにヒープh0を与えると、
最小要素を含むツリーtと残りのヒープhが生成されるとする。
また、このときのヒープh0は、要素数n、内部の0の数はk0とする。
また、最小値はrankがrのツリーに含まれ、tのランクはrだったとする。

すると、removeMinTreeの入力と出力のポテンシャルは以下のようになる。

1. h0のポテンシャル
    - k0
2. hのポテンシャル
    - k0 - 1
3. tのポテンシャル
    - r - 1

よってポテンシャルの変化量は
  (k0 - 1) + (r - 1) - k0 = r - 2

次にremoveMinTreeの完全コストを考える。
removeMinは実装より、ヒープhが持つすべてのツリーを操作する。
よって、ceil(log(n))に比例したコストがかかる。

よって、removeMinTreeかかる償却コストは、

ceil(log(n)) - r - 2

最悪時間はr=0の場合である。
よって、findMinの償却コストはO(log(n))である

# deleteMinの償却コストがO(log(n))である証明

deleteMinはdeleteMinと違い、removeMinTreeのあとにも処理がある。
この処理には追加のmrg呼び出しがある。

この呼び出しには、まず(rev ts1)で長さrのリストをリバースする処理がある。
次にmrgによって、最悪ヒープh0に対してO(log(n0))かかる。

よって、deleteMinの償却コストは、

  (r + ceil(log(n0))) + ceil(log(n)) - r - 2
= ceil(log(n0)) + ceil(log(n)) - 2

よってO(log(n))である。

# mergeの償却コストがO(log(n))である証明

TODO
