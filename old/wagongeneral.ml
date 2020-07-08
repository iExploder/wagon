module WagonGeneral = struct

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
type ('a, 'b) ws = (string -> 'a) -> 'b

let _cons_none : ('b, 'c) ws -> ('a, 'b) ws -> ('a, 'c) ws =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ sb)))

let _cons_comma : ('b, 'c) ws -> ('a, 'b) ws -> ('a, 'c) ws =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ ", " ^ sb)))

let _lbk  : ('a, 'a) ws = fun k -> k "{"
let _rbk  : ('a, 'a) ws = fun k -> k "}"

let _make_cps : ('a, 'b) wtype -> ('c, 'b -> 'c) ws =
  fun wt -> fun k v -> k (wt.init v)

let typ = _make_cps
let (<>) = _cons_comma

let wstruct : ('a, 'b) ws -> (('a, 'b) ws) wterm =
fun s -> (_cons_none (_cons_none _lbk s) _rbk) (fun x -> x)

end