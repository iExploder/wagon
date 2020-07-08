open Printf

type wtname = string
type 't wterm = string
type ('a, 'b) wfus = (wterm -> 'a) -> 'b

let _cons : ('b, 'c) wfus -> ('a, 'b) wfus -> ('a, 'c) wfus =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ sb)))
let _lbk  : ('a, 'a) wfus = fun k -> k "{"
let _rbk  : ('a, 'a) wfus = fun k -> k "}"

let (@>) : ('b, 'c) wfus -> ('a, 'b) wfus -> ('a, 'c) wfus =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ ", " ^ sb)))

let wtnamei : wtname -> int -> wterm =
  fun tn i -> sprintf "(%s)%d" tn i
let wtnamef : wtname -> float -> wterm =
  fun tn f -> sprintf "(%s)%f" tn f
let wtyp : ('a -> wterm) -> ('b, 'a -> 'b) wfus =
  fun t -> fun k v -> k (t v)

(*
So How to init an array with an OCaml array?

How to achieve like
arr wi8 20 -> (wterm -> 'a) -> int array -> wterm
*)

type wi8
type wi16
type wi32
type wi64
type wf32
type wf64

let wi8  : ('a, int   -> 'a) wfus = fun k i -> k (sprintf "(int8_t)%d" i)
let wi16 : ('a, int   -> 'a) wfus = fun k i -> k (sprintf "(int16_t)%d" i)
let wi32 : ('a, int   -> 'a) wfus = fun k i -> k (sprintf "(int32_t)%d" i)
let wi64 : ('a, int   -> 'a) wfus = fun k i -> k (sprintf "(int64_t)%d" i)
let wfus32 : ('a, float -> 'a) wfus = fun k f -> k (sprintf "%0.3ff" f)
let wfus64 : ('a, float -> 'a) wfus = fun k f -> k (sprintf "%0.6f" f)

let wfustr : ('a, 'b) wfus -> 'b =
  fun x -> (_cons (_cons _lbk x) _rbk) (fun a -> a)


open Printf

type wtname = string
(* Designed to be ... wtype wterm *)
type 'a wterm = string

type ('t, 'init) wtype = {typename: wtname; init: 'init -> 't wterm}

type wi8
type wi16
type wi32
type wi64
type wf32
type wf64

let wi8    : (wi8, int) wtype = {
  typename = "int8_t";
  init = fun i -> sprintf "(int8_t)%d" i
}
let wi16   : (wi16, int) wtype = {
  typename = "int16_t";
  init = fun i -> sprintf "(int16_t)%d" i
}
let wi32   : (wi32, int) wtype = {
  typename = "int32_t";
  init = fun i -> sprintf "(int32_t)%d" i
}
let wi64   : (wi64, int) wtype = {
  typename = "int64_t";
  init = fun i -> sprintf "(int64_t)%d" i
}
let wf32   : (wf32, float) wtype = {
  typename = "float";
  init = fun f -> sprintf "%0.3ff" f
}
let wf64  : (wf64, float) wtype = {
  typename = "double";
  init = fun f -> sprintf "%0.6f" f
}
let warr  : ('t, 'i) wtype -> int -> ('t array, 'i array) wtype = 
  fun t n -> {
    typename = sprintf "%s[%n]" t.typename n;
    init = fun a -> 
      if Array.length a <= n 
      then 
        let dirty_str = Array.fold_left (fun a b -> sprintf "%s, %s" a (t.init b)) "" a
        in sprintf "{%s}" @@ String.sub dirty_str 2 ((String.length dirty_str) - 2)
      else failwith "unexpected array length" 
  }
type ('a, 'b) wfus = (string -> 'a) -> 'b

let _cons_none : ('b, 'c) wfus -> ('a, 'b) wfus -> ('a, 'c) wfus =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ sb)))

let _cons_comma : ('b, 'c) wfus -> ('a, 'b) wfus -> ('a, 'c) wfus =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ ", " ^ sb)))

let _lbk  : ('a, 'a) wfus = fun k -> k "{"
let _rbk  : ('a, 'a) wfus = fun k -> k "}"

let _make_cps : ('a, 'b) wtype -> ('c, 'b -> 'c) wfus =
  fun wt -> fun k v -> k (wt.init v)

let typ = _make_cps
let (<>) = _cons_comma

let wfustruct : ('a, 'b) wfus -> (('a, 'b) wfus) wterm =
fun s -> (_cons_none (_cons_none _lbk s) _rbk) (fun x -> x)

let (<>) = 
fun t0 t1 -> {}