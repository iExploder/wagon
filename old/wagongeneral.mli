module type WagonGeneral =
  sig
    type 't wexpr
    type ('e, 'a, 'b) wfus_e = ('e wexpr -> 'a) -> 'b
    type ('at, 'wfa, 'wfb) wtype = {
      name : string;
      init : ('at, 'wfa, 'wfb) wfus_e;
    }
    val cons_finit_in :  ('e1, 'b, 'c) wfus_e -> ('e2, 'a, 'b) wfus_e -> ('e1 * 'e2, 'a, 'c) wfus_e
    val cons_wtype :
      ('at0, 'wfb, 'wfc) wtype ->
      ('at1, 'wfa, 'wfb) wtype -> ('at0 * 'at1, 'wfa, 'wfc) wtype
    type wi8
    type wi16
    type wi32
    type wi64
    type wf32
    type wf64
    type 't warray
    type 't wstruct
    val wi8 : (wi8, 'a, int -> 'a) wtype
    val wi16 : (wi16, 'a, int -> 'a) wtype
    val wi32 : (wi32, 'a, int -> 'a) wtype
    val wi64 : (wi64, 'a, int -> 'a) wtype
    val wf32 : (wf32, 'a, float -> 'a) wtype
    val wf64 : (wf64, 'a, float -> 'a) wtype
    val init : ('a, 'a wexpr, 'b) wtype -> 'b
  end