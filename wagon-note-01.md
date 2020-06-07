# Wagon Development Note 0x01: Limitation of General Type (Extended)

Wagon is designed as an eDSL(Embedded DSL) of OCaml,
for developing high-performance computing programs taking advantage of
Intel SIMD architecture.

## Fundamental Design of **Wagon**

### Pros & Cons

In design of Wagon, you may benefit from **Wagon**'s pros:

* SIMD optimized performance on functional programming language(OCaml)
* Static Type Safety (to avoid runtime errors) 
  * (Notes below)
  * Need examples(void* -> some*)
  * Safe program -> Safe program
  * Program with (void*) -> Available in Wagon with substitude
  * Dangerous (but useful) C Code with no substitude in Wagon is **NOT** welcomed
* No Implicit Type Casting by default (to avoid unconcerned type-casting caused errors)
* Aligned SIMD-friendly vector types

(Note: To be added into design, examples needed)

* Function Composition Available (**) --to Top
  * f (g x) ...
* Abstraction (Polymorphism, ...)     --to Top
  
Besides, your programming may be affected by **Wagon**'s drawbacks:

* No automatic vectorization yet (you vectorize your code manually)
* (For Former-Users of C, ...) Functional Programming may be different from Imperative Programming

### Intended Users' Image

* Functional Programmers who want their program run faster on modern arch.
* Imperative Programmers who like introducing functional programming into their projects.

### Main Feature

```text
+---------------+                  +---------------------------------+
| Pre-processed |  Tagless-final   | C Code (in string)              |
|   Wagon Code  | ===============> | (Primitive & Compositive Types) |
|     (eDSL)    |  Code Generator  | with Intel SIMD Intrinsics      |
+---------------+                  +---------------------------------+
```

### Structure

```plaintext
+--------------------------------------+                 +-----------------------------------+
| module type                          |   implement     | module WagonGeneral               |
| WagonBasic                           | =============>  | Generates Main Codes              |
+--------------------------------------+        |        +-----------------------------------+
| * Primitive Types                    |        |        | module WagonDeclare               |
| * Compositive Type Constructors      |        ======>  | Generates Declaration (Types ...) |
| * Compositive Type Member Accessors  |                 +-----------------------------------+
| * Function Constructors              |  
+--------------------------------------+
```
Notice: Terms in WagonBasic are all applied to remote types.

The topic is still about remote types(types in generated C code) representation
in OCaml.

## Trial-and-error

### Prologue

First, the key point at the very beginning is to use **functional unparsing** to
achieve **building a C-`struct`**.

```ocaml
type ('a, 'b) wfus = (string -> 'a) -> 'b
```

In this code, `wfus` means "Wagon's Functional Unparsing style Serializer".

### Gotcha #1: Increased general type usage in type definition

I tested making a record type with polymorphic type function inside.

```ocaml
type 'i winit_nofa = {f : ('a, ('i -> 'a)) wfus}
```

Of course it failed.

```text
utop # type 'i winit_nofa = {f : ('a, ('i -> 'a)) wfus};;
Error: Unbound type parameter 'a
```

I discussed and searched and found my mistake is a missing **FORALL** sign.

```ocaml
type 'i winit_fa = {fa : 'a. ('a, ('i -> 'a)) wfus}
```

For all `'a`, `fa` is `('a, ('i -> 'a)) wfus`.
This makes sense.

### Gotcha #2: Limited expression

In code below, reversable transformation of 
**functional unparsing** style and
**plain** style initializers are implemented.

```ocaml
type 'i winit = {f : 'a. ('a, 'i -> 'a) wfus}
type 'i finit = 'i -> string

let winit : 'i finit -> 'i winit =
  fun fx -> 
    {f = fun k v -> k (fx v)}

let finit : 'i winit -> 'i finit =
  fun wf ->
    wf.f (fun x -> x)
```

In further testing,
the code can do everything well,
except for **correctly handling combination type**, 
which is an essential usage of **functional unparsing**.

What a pity!

So a solution is to change `init` function in `wtype`(remote type representation),
into real functional unparsing **format** style. 

Besides, from now on, abstract types as type system "tags" are used as below.

```ocaml
type wi8
type wi16
type wi32
type wi64
type wf32
type wf64

type 't wstruct
```


### Gotcha #3: Losing Polymorphism in stacked `let ... in`

For convenience in implementing by C&P(Copy & Paste), 
please look at this piece of code.

```ocaml
let wi8   : (wi8,  'a, int -> 'a) wtype' = 
{name = "int8_t";  init = let vf = sprintf "(int8_t)%d" in fun k v -> k (vf v)}
```

```text
val wi8 : (wi8, '_a, int -> '_a) wtype' =
  {name = "int8_t"; init = <fun>}
```

The `init` member function just loses polymorphism.
But things change if we change the writting style as below.

```ocaml
let wi8'  : (wi8,  'a, int -> 'a) wtype' = 
{name = "int8_t";  init = fun k v -> k (sprintf "(int8_t)%d" v)}
```

```text
val wi8' : (wi8, 'a, int -> 'a) wtype' =
  {name = "int8_t"; init = <fun>}
```

It seems that nothing changed from `wi8`.
But the result shows it keeps polymorphism in `init`.

Taking the member function out alone,
maybe we can find where is the problem.

```ocaml
let init = let vf = sprintf "(int8_t)%d" in fun k v -> k (vf v);;
val init : (wterm -> '_weak1) -> int -> '_weak1 = <fun>

let init' = fun k v -> k (sprintf "(int8_t)%d" v);;
val init' : (wterm -> 'a) -> int -> 'a = <fun>

let init'' = let vf = fun i -> "(int8_t)" ^ (string_of_int i) in fun k v -> k (vf v);;
val init'' : (wterm -> 'a) -> int -> 'a = <fun>

let init''' = let vf = fun i -> sprintf "(int8_t)%d" i in fun k v -> k (vf v);;
val init''' : (wterm -> 'a) -> int -> 'a = <fun>
```

After discussion in "Kayoukai"(Tuesday-gang's meeting),
the key point of this problem is **Value Restriction**.

It depends on when the type inference is applied.
In case `init`, `vf` is function application, type of `vf` is evaluated after the type inference.