# 2020/06/24 個人ミーティング

## Reusing `WagonBasic` module type

* Not fully type-safe AST for C language
* Toplevel -> `main()`
  * Pseudo-lambda expression
    * **Declare list**(outside `main()`)
    * Declaration of lambda-style function =>
      * Using generated function name in `main()`
      * Actual function body appended to **Declare list** 
  * Local variables used in functions
  * How to make scope?

## 中間発表

## 日本ソフトウェア科学会