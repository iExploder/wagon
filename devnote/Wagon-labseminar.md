# Wagon: Type-safe Accelerated DSL for OCaml

## Introduction: Background

Recently, SIMD instructions are widely brought in on modern x86-based architectures,
for enhancing single-core performance.

From Intel's Ice Lake architecture for consuming market and
Knights Landing(Xeon Phi x200) / Skylake-X(Core-X Series, Xeon Scalable Processor Family)
for enterprise and workstations,
SIMD instructions with up to 512-bit vectors as operands are supported.

However, SIMD instructions are not designed as out-of-the-box.

Automatic vectorization-based optimization implemented in modern compilers are far from squeezing
all the juice of modern CPUs.
Besides, writing vectorized assembly code with manual management of registers is not reasonable.

There comes a balanced solution, **SIMD Intrinsics**, instructions in form of C functions.

With SIMD Intrinsics, you CAN

* Do vectorized operations without management of registers
* Do manual vectorization-based optimization on your C/C++ (or languages with compatibility with C language) programs

You CAN NOT

* Take advantage of automatic SIMD vectorization by compilers (since analysis become complicated on manually vectorized codes)

There are several problems on usage of Intel SIMD instructions, such as

* **Memory alignment problem**. 128-bit SIMD instructions have not aligned version with approximately 20% of performance loss, while SIMD instructions over 256-bit require all operands be aligned to multiple of vector lengths.

```text
(Memory Space)
+-+-+-+-+-+-+-+-+      +-+-+-+-+-+-+-+-+
| | | | | | | | |      | | | | | | |O|O|
+-+-+-+-+-+-+-+-+      +-+-+-+-+-+-+-+-+      +-+
|O|O|O|O|O|O|O|O|      |O|O|O|O|O|O|X|X|      | | = 64 bits
+-+-+-+-+-+-+-+-+      +-+-+-+-+-+-+-+-+      +-+
|X|X|X|X|X|X|X|X|      |X|X|X|X|X|X| | |
+-+-+-+-+-+-+-+-+      +-+-+-+-+-+-+-+-+
       OK!           NG! (Segmentation Fault)
When doing something SIMD(512-bit) with Vectors O & X
```

* **Unioned Integer Vectors**. Only **ONE** type of vector for each length handling all flavors of integer scalars in `union`s. Developers must remember scalar member type of integer vectors.
  
```c
/* From <immintrin.h> */
typedef union  __declspec(intrin_type) __declspec(align(32)) __m256i {
    __int8              m256i_i8[32];
    __int16             m256i_i16[16];
    __int32             m256i_i32[8];
    __int64             m256i_i64[4];
    unsigned __int8     m256i_u8[32];
    unsigned __int16    m256i_u16[16];
    unsigned __int32    m256i_u32[8];
    unsigned __int64    m256i_u64[4];
} __m256i;
```

* **Unconcerned Type Casting**. This problem is actually from C language, not SIMD intrinsics. 
  In C language **implicit type casting** is enabled default and causing unconcerned problems.

## Fundamental Design of **Wagon**

There comes the design of **Wagon** to solve the problems above.

### Design(on Roadmap) for the problems

* For **Memory alignment problem**, arrays and vectors aligned or aligned data through alignment-preserving operations **CAN** be used in vectorized intrinsic functions.
  * Vector types are properly aligned by default
  * Optional aligned arrays (on purpose)
  * Sub-array & Sub-vector functions preserves alignment tags
* For **Unioned Integer Vectors**, unioned access for integer vectors is **DISABLED** in Wagon.
  Instead, combinations for all integer types and vector lengths are provided. 
  Besides, transformation for different integer flavors of vectors are provided
  for purposed operations.

```text
+---------------+                  +---------------------------------+
| Pre-processed |  Tagless-final   | C Code (in string)              |
|   Wagon Code  | ===============> | (Primitive & Compositive Types) |
|     (eDSL)    |  Code Generator  | with Intel SIMD Intrinsics      |
+---------------+                  +---------------------------------+
```

### Pros & Cons

In design of Wagon, you may benefit from **Wagon**'s pros:

* SIMD optimized performance on functional programming language(OCaml)
* Static Type Safety
* No Implicit Type Casting by default
  * Unconcerned type castings are prevented
  * Type coercing also provided for experienced SIMD developers
* Aligned SIMD-friendly vector types
  * Data with SIMD-available access are correctly aligned

Besides, your programming may be affected by **Wagon**'s drawbacks:

* No automatic vectorization yet (you vectorize your code manually)
* (For Former-Users of C, ...) Functional Programming may be different from Imperative Programming

### Intended Users' Image

* Functional Programmers who want their program run faster on modern architecture.
* Imperative Programmers who like adding functional programming flavors into their projects.

## Highlights

### Tagless-final: Base of the language

```ocaml
(* (G)ADT example *)
type expr =
| IntLit of int
| Add of expr * expr
let rec eval e = 
match e with
| IntLit(i) -> i
| Add(e1, e2) -> (eval e1) + (eval e2)
let rec display e =
match e with
| IntLit(i) -> string_of_int i
| Add(e1, e2) -> Printf.sprintf "(%s + %s)" 
				   (display e1) (display e2)
let e = Add(IntLit 1, IntLit 2)
let result = eval e 
(* -> int : 3 *)
let str_expr = display e 
(* string: "(1 + 2)" *)

(* Tagless-final example *)
module type TFExample = sig
    type expr
    val intlit : int -> expr
    val add : expr -> expr -> expr
end
module TFEval = struct
    type expr = int
    let intlit = fun x -> x
    let add = fun e1 e2 -> e1 + e2
end
module TFDisplay = struct
    type expr = string
    let intlit = fun x -> string_of_int x
    let add = fun e1 e2 -> Printf.sprintf "(%s + %s)" e1 e2
end
module Expr(T) = struct
    open T
    let s = add (intlit 1) (intlit 2)
end
let result = let module Eval = Expr(TFEval) in Eval.s
let str_expr = let module Display = Expr(TFDisplay) in Display.s
```

### Functional Unparsing for Painless Support of Combinated Data Types



From Danvy(1998) 's problem of creating type-safe functional formatted print function
like `printf()` in C language.
The solution used Continuation Passing Style(CPS) to implement combinable format elements in higher-order functions.

Here is an example.
```ocaml
type ('a, 'b) fmt = (string -> 'a) -> 'b

(* Literal String & Sample Formats *)
let lit : string -> ('a, 'a) fmt = fun s -> fun k -> k s
let int : ('a, int -> 'a) fmt = fun k x -> k (string_of_int x)

(* Combinator Operator *)
let (<>) : ('b, 'c) fmt -> ('a, 'b) fmt -> ('a, 'c) fmt =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ sb)))

let my_sprintf : (string, 'b) fmt -> 'b = fun f -> f (fun x -> x)

(* Usage *)
# my_sprintf (lit "x=" <> int) 3;;
- : string = "x=3"
```

Here gives (manual) type inference of how `<>` works.

```ocaml
let (<>) : ('b, 'c) fmt -> ('a, 'b) fmt -> ('a, 'c) fmt =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ sb)))

sa : string
sb : string

l : ('b, 'c) fmt <=> (string -> 'b) -> 'c
r : ('a, 'b) fmt <=> (string -> 'a) -> 'b
k : string -> 'a
k (sa ^ sb) : 'a 
(fun sb -> k (sa ^ sb)) : string -> 'a
(fun sa -> r (fun sb -> k (sa ^ sb))) : string -> 'b
```

So for combinating `float` and `int`,
```ocaml
float : ('x, float -> 'x) fmt
int : ('y, int -> 'y) fmt
```
* From `float`, `'b = 'x`, `'c = float -> x`.
* From `int`, `'a = 'y`, `'b = int -> y`.
* We can find
  * `'a = 'y`
  * `'b = int -> 'y`
  * `'c = float -> 'x = float -> int -> 'y`
* So combined format type is
`('a, 'c) fmt = ('y, float -> int -> 'y) fmt`.

In this way we could achieve combined formats.
In other words, literal in string of combined types could be also  

## Roadmap

* Remote Type Representation (DONE)
  * Functional Unparsing Style
* Function Representation (NEXT)