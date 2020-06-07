# Wagon Development Note 0x02: Declarations of Variables and Functions

## Building typed expressions

Let us see the definition of `wtype`.
```ocaml
type ('a, 'b) wfus = (string -> 'a) -> 'b
type ('at, 'wfa, 'wfb) wtype = {
name : string;
init : ('wfa, 'wfb) wfus;
}
```
Since `wfus` type is defined in **Functional Unparsing** style,
types could be combined as tuples (or encapsulated as C-`struct`s, further).

In a typical implementation of **expression** in tagless final style,
abstract type of `'a expr` in module signature with actual `string` implementation is usually used for taking advantage of type system of the host functional programming language.

## 1st Trial: Naive approach

What we want in this scenario is a function with type

```
val init : (typ, _, _) wtype -> initializer type -> typ wexpr
```

For example,

```
init wi8 : int -> wi8 wexpr
```

So a naive approach is tested below.


```ocaml
let init = fun (typ: ('t, _, _) wtype) -> 
let f : (string -> 't wexpr) = fun x -> x in 
typ.init f
```
```text
val init : ('t, string, 'a) wtype -> 'a = <fun>
```
```ocaml
# init wi8;;
- : int -> string = <fun>
```
Not working as expected.

## 2nd Trial: Increased Type Parameters

Then I concerned about type definition of `wfus` and added a patch to it.

```ocaml
type ('t, 'a, 'b) wfus' = ('t wexpr -> 'a) -> 'b
type ('t, 'a, 'b) wtype' = {
name : string;
init : ('t, 'wfa, 'wfb) wfus;
}
```

And the combinator should be changed too.

```ocaml
let cons_finit_in : ('e1, 'b, 'c) wfus_e -> ('e2, 'a, 'b) wfus_e -> ('e1 * 'e2, 'a, 'c) wfus_e =
fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ ", " ^ sb)))

let cons_wtype :
  ('at0, 'wfb, 'wfc) wtype ->
  ('at1, 'wfa, 'wfb) wtype -> ('at0 * 'at1, 'wfa, 'wfc) wtype =
  fun a b -> {
  name = sprintf "%s, %s" a.name b.name;
  init = cons_finit_in a.init b.init
  }

let init = fun x -> x.init (fun a -> a)
```

Of course it works as designed.

```text
val init : ('a, 'a wexpr, 'b) wtype -> 'b = <fun>
# init wi32;;
- : int -> WagonGeneral.wi32 WagonGeneral.wexpr = <fun>
```

## 3rd Trial(Now): Enhanced Original Design

After the patch, I noticed if the increased type parameter is
actual unnecessary for this feature.

Look at the definition of `wfus` type,
```ocaml
type ('a, 'b) wfus = (string -> 'a) -> 'b
```
wi16
wi16_init : (wi16 wexpr, 'b) = (string -> wi16 wexpr) -> 'b
fun x -> x : ('a -> 'a) 

If `wfus` type do not contains abstract
