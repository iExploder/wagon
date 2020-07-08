type ('a, 'b) wfus = (wterm -> 'a) -> 'b

(* Without FORALL type declaration *)
type 'i winit_nofa = {f : ('a, ('i -> 'a)) wfus}
(* NOT WORKING!! *)

(* With FORALL type declaration *)
type 'i winit_fa = {fa : 'a. ('a, ('i -> 'a)) wfus}
(* WORKING!! *)


open Printf
(* Wagon's Functional-Unparsing style Serializer *)
type ('a, 'b) wfus = (wterm -> 'a) -> 'b

type 'i winit = {f : 'a. ('a, 'i -> 'a) wfus}
type 'i finit = 'i -> string

let winit : 'i finit -> 'i winit =
  fun fx -> 
    {f = fun k v -> k (fx v)}

let finit : 'i winit -> 'i finit =
  fun wf ->
    wf.f (fun x -> x)

(* Abstract Type, initializer Type *)
type ('at, 'x) wtype = {name : string; init : 'x finit}

(* Abstract Types *)
type wi8
type wi16
type wi32
type wi64
type wf32
type wf64

let wi8  : (wi8, int) wtype = 
{name = "int8_t"; init = sprintf "(int8_t)%d"}

(* wi16~wi64 omitted *)

let wf32 : (wf32, float) wtype = 
{name = "float";  init = sprintf "%0.3ff"}
let wf64 : (wf64, float) wtype = 
{name = "double"; init = sprintf "%0.6f"}


(* Version 2 *)

type wterm = string
type ('a, 'b) wfus' = (string -> 'a) -> 'b
type ('at, 'wfa, 'wfb) wtype' = {name: string; init: ('wfa, 'wfb) wfus'}

let cons_finit_in : ('b, 'c) wfus' -> ('a, 'b) wfus' -> ('a, 'c) wfus' =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ ", " ^ sb)))
let cons_wtype' : ('at0, 'wfb, 'wfc) wtype' -> ('at1, 'wfa, 'wfb) wtype' 
               -> (('at0 * 'at1), 'wfa, 'wfc) wtype' =
fun a b -> {
  name = sprintf "%s, %s" a.name b.name;
  init = cons_finit_in a.init b.init
}

(* Abstract Types *)
type wi8
type wi16
type wi32
type wi64
type wf32
type wf64

type 't warray

type 't wstruct

let wsm0 : (('a * 'b) wstruct, _, _) wtype -> ('a, _, _) wtype 
"ws_0123abcd.v0"

let wi8   : (wi8,  'a, int -> 'a) wtype' = 
{name = "int8_t";  init = fun k v -> k (sprintf "(int8_t)%d" v)}
let wi16  : (wi16, 'a, int -> 'a) wtype' = 
{name = "int16_t"; init = fun k v -> k (sprintf "(int16_t)%d" v)}
let wi32  : (wi32, 'a, int -> 'a) wtype' = 
{name = "int32_t"; init = fun k v -> k (sprintf "(int32_t)%d" v)}
let wi64  : (wi64, 'a, int -> 'a) wtype' = 
{name = "int64_t"; init = fun k v -> k (sprintf "(int64_t)%d" v)}

let wf32 : (wf32, 'a, float -> 'a) wtype' = 
{name = "float";  init = fun k v -> k (sprintf "%0.3ff" v)}
let wf64 : (wf64, 'a, float -> 'a) wtype' = 
{name = "double"; init = fun k v -> k (sprintf "%0.6f" v)}

open Hashtbl
let _cons_none : ('b, 'c) wfus' -> ('a, 'b) wfus' -> ('a, 'c) wfus' =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ sb)))
let _lbk  : ('a, 'a) wfus' = fun k -> k "{"
let _rbk  : ('a, 'a) wfus' = fun k -> k "}"

let wstruct : ('sl, 'a, 'b) wtype' -> ('sl wstruct, 'a, 'b) wtype' =
fun x -> {
  name = sprintf "ws_%08x" (hash x.name); 
  init = _cons_none (_cons_none _lbk x.init) _rbk
}

type 'a decl = {v : 'a expr ref; output : stmt}
let declare : (_, _, _) wtype -> _ decl
let assign : _ expr ref -> _ expr -> stmt

let (@+) = cons_wtype 

let struct1 = wstruct (wi8 @+ wi64 @+ wf64)
let var1 = declare wi8
let var2 = declare struct1 


let init : (_, 'a, 'b) wtype' -> 'b =
fun x -> x.init (fun a -> a)



