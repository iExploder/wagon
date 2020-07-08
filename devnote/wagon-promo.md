# Wagonの実装による問題点

## Functional Unparsing(Danvy+ ’98)

* 関数型言語による(s)printf(format, args)の実装問題
  * 組合せ可能なフォーマット型
  * フォーマット型
    * `sprintf intF : int -> string`
    * `sprintf floatF : float -> string`
  * 組合せ可能性
    * `sprintf (intF <> floatF) : int -> float -> string`
    * 但し、<>は組合せ演算子
* 結論
  * フォーマット型:	type (’a, ’b) fmt = (string -> ’a) -> ’b
  * 整数フォーマット:	val intF : (’a, int -> ’a) fmt
  * 組合せ演算子:	val (<>) : (’b, ’c) fmt -> (’a, ’b) fmt -> (’a, ’c) fmt

## なぜ組み合わせ可能？

* (a) 型A: val fmt_a : ('x, 'fa... -> 'x) fmt
* (b) 型B: val fmt_b : ('y, 'fb... -> 'y) fmt
* (c) 組合せ演算子: val (<>) : (’b, ’c) fmt -> (’a, ’b) fmt -> (’a, ’c) fmt
* 注記： 'x, 'y, 'a, 'b, 'cは多相型であり、
      'fa...と'fb...は型Aと型Bの具体的な型パラメータである。
* (a), (b) -> (c):
  * (d) 'b == 'x, 'c == 'fa... -> 'x
  * (e) 'a == 'y, 'b == 'fb... -> 'y
* (e) -> (d):
  * 