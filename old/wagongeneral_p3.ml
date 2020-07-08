module WagonGeneral : WagonGeneral = struct
  open Printf
  type 'a wexpr = string
  type ('e, 'a, 'b) wfus_e = ('e wexpr -> 'a) -> 'b
  type ('at, 'wfa, 'wfb) wtype = {
    name : string;
    init : ('at, 'wfa, 'wfb) wfus_e;
  }
  let cons_finit_in : ('e1, 'b, 'c) wfus_e -> ('e2, 'a, 'b) wfus_e -> ('e1 * 'e2, 'a, 'c) wfus_e =
    fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ ", " ^ sb)))

  let cons_wtype :
    ('at0, 'wfb, 'wfc) wtype ->
    ('at1, 'wfa, 'wfb) wtype -> ('at0 * 'at1, 'wfa, 'wfc) wtype =
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

  type 'i ityp

  type 't warray

  type 't wstruct

  let wi8   : (wi8 ityp,  'a, int -> 'a) wtype =
  {name = "int8_t";  init = fun k v -> k (sprintf "(int8_t)%d" v)}
  let wi16  : (wi16 ityp, 'a, int -> 'a) wtype =
  {name = "int16_t"; init = fun k v -> k (sprintf "(int16_t)%d" v)}
  let wi32  : (wi32 ityp, 'a, int -> 'a) wtype =
  {name = "int32_t"; init = fun k v -> k (sprintf "(int32_t)%d" v)}
  let wi64  : (wi64 ityp, 'a, int -> 'a) wtype =
  {name = "int64_t"; init = fun k v -> k (sprintf "(int64_t)%d" v)}

  let wf32 : (wf32, 'a, float -> 'a) wtype =
  {name = "float";  init = fun k v -> k (sprintf "%0.3ff" v)}
  let wf64 : (wf64, 'a, float -> 'a) wtype =
  {name = "double"; init = fun k v -> k (sprintf "%0.6f" v)}
  let init = fun x -> x.init (fun a -> a)
end