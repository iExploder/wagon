# Wagon Development Note 0x03: Function Type Design 関数型の設計

## Prelude

現状として、`wtype`は以下の構造を持つ。
```ocaml
type ('at, 'wfa, 'wfb) wtype = {
  name : string;
  init : (string -> 'wfa) -> 'wfb
}
```
* `'at` is **Abstract** type used as type tag for type checking taking advantage of OCaml's type system.
* `'wfa, 'wfb` are type parameters for composable serializer function in **Functional Unparsing** style.
* Member `name` is type name in target language (C language).
* Member `init` is composable serializer generates **string** of a target term.

* `'at`は抽象型であり、型タグ(OCaml型検査系用)である。
* `'wfa, 'wfb`はFunctional Unparsingの組合せ可能な文字列化関数の型パラメータである。
* メンバー`name`はターゲット言語(C言語)による**型名**である。
* メンバー`init`はこのデータ型に相応な、ターゲット言語による項を**文字列**の形で生成する組合せ可能な文字列化関数である。

## Function Type Design 関数型の設計

* Taking advantage of CPS'd Functional Unparsing 
* Unified **Function** & **General** types(Wagon's `wtype` type)


* 継続渡し方式Functional Unparsingを活用
* 一般データ型と関数型の整合


### Functional Function Type? 関数型な関数型？

関数型な関数型といえば、
カリー化可能な関数型を指している。

通常なTagless-final式実装では、
```ocaml
type 'a repr
val lam : ('a repr -> 'b repr) -> ('a -> 'b) repr
val app : ('a -> 'b) repr -> 'a repr -> 'b repr
```
のラムダ(Lambda)演算子と適用(Application)演算子と、四則演算と必要な基本操作があれば、
型無し(typeless)なチューリング完全言語ができる。

一言でいうと、以上の条件を全部満たす関数型の型設計は
```ocaml
('at0, 'a0, 'b0) wtype -> ('at1, 'a1, 'b1) wtype -> ('at0 -> 'at1, 'a0, ('at0 wexpr -> 'at1 wexpr) -> 'a0) wtype
```
のようなものである。
つまりOCamlの関数型な関数構造と近い、組み合わせ可能な関数型を作れればうれしいとなる。

何度も奮闘したけど、一番目の難題は

* 基本型 T : 'at部に矢印を含まない型　
例：`(wi32, 'a, int -> 'a) wtype`
* 部分関数型 F : 'at部に矢印を含む型
例：`(wi32 -> wi32, 'a, (wi32 wexpr -> wi32 wexpr) -> 'a) wtype`

上記のTとFに対し、統合された演算子`@->`を作ることは必要となる。
使用シーンを分析したら、
* T @-> T => F
* T @-> F => F
計2種類の場合がある。

`wi32`型を例として、
`val (@->)`では、
以下の性質を満たなければならない。

* `(wi32, 'a, int -> 'a) wtype -> (wi32, 'a, int -> 'a) wtype -> (wi32 -> wi32, 'a, (wi32 wexpr -> wi32 wexpr) -> 'a) wtype`
* `(wi32, 'a, int -> 'a) wtype -> (wi32 -> wi32, 'a, (wi32 wexpr -> wi32 wexpr) -> 'a) wtype -> (wi32 -> wi32 -> wi32, 'a, (wi32 wexpr -> wi32 wexpr -> wi32 wexpr) -> 'a) wtype`

結論として、型の実装は困難である。

### C-style Function Type? C言語風関数型？

改めて、ターゲット言語の文法と近いカリー化不可能な、一般的な命令式言語の関数型を一案を考える。

`int func(int x, float y)`の形の関数を考えましょう。
OCamlで書けば、`tuple -> return_type`に制限された関数の設計である。

しかし、引数リストを文字列化したら、昔現れた問題と似ている問題がある。

* トラバーサル可能なヘテロジニアスリスト
* またFunctional Unparsingするの？