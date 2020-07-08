module type WagonBasic =
  sig
    type 't wexpr
    type ('a, 'b) wfus
    type ('at, 'wfa, 'wfb) wtype

    type wstmt

    (* L-value in C *)
    type 't lval

    type 't decl
    type 't wfunc

    val cons_finit_in :  ('b, 'c) wfus -> ('a, 'b) wfus -> ('a, 'c) wfus
    val cons_wtype :
      ('at0, 'wfb, 'wfc) wtype ->
      ('at1, 'wfa, 'wfb) wtype -> ('at0 * 'at1, 'wfa, 'wfc) wtype

    val cons_wtype_paramlist :
      ('at0, 'wfb, 'wfc) wtype ->
      ('at1, 'wfa, 'wfb) wtype -> ('at0 * 'at1, 'wfa, 'wfc) wtype
      
    type wbln
    type wi8
    type wi16
    type wi32
    type wi64
    type wf32
    type wf64

    type 'i ityp

    type 't warray
    type 't wstruct
    val wbln : (wbln ityp, 'a, bool  -> 'a) wtype
    val wi8  : (wi8  ityp, 'a, int   -> 'a) wtype
    val wi16 : (wi16 ityp, 'a, int   -> 'a) wtype
    val wi32 : (wi32 ityp, 'a, int   -> 'a) wtype
    val wi64 : (wi64 ityp, 'a, int   -> 'a) wtype
    val wf32 : (wf32, 'a, float -> 'a) wtype
    val wf64 : (wf64, 'a, float -> 'a) wtype

    val lit : ('a, 'a wexpr, 'b) wtype -> 'b
    val wstruct : ('th * 'tl, 'a, 'b) wtype -> (('th * 'tl) wstruct, 'a, 'b) wtype
    val warray : ('at, 'a, 'b) wtype -> int -> ('at warray, 'c, 'at wexpr array -> 'c) wtype

(* 

X:  lit @@ warray wi16 [|1; 100|] 
O:  lit @@ warray wi16 [|lit wi16 1; lit wi16 100|]

warray wi16 2:
(wi16 ityp warray, 'a, wi16 ityp wexpr array -> 'a) wtype
={
  name= "int16_t[2]";
  init = fun k arr -> 
  (* Length Check Needed *)
  let lst = Array.to_list arr in
  k ("{" ^ String.concat ", " lst ^ "}")
}

lit (warray wi16 2):
wi16 ityp wexpr array -> wi16 ityp warray wexpr 

lit (warray wi16 2) [|lit wi16 1; lit wi16 100|] => "{1,100}"

get-er, set-er for arrays, structures.
*)
    val warray_get : 'a warray wexpr -> 'b ityp wexpr -> 'a wexpr
    val warray_set : 'a warray wexpr -> 'b ityp wexpr -> 'a wexpr -> wstmt
    val wfunc : ('at0, 'a0, 'b0) wtype -> ('at1, 'a1, 'b1) wtype -> ('at0 -> 'at1, 'a0, 'at0 wexpr -> 'at1 wexpr -> 'a0) wtype
    val show : 'a wexpr -> string

    (* Remove lval tag *)
    val delval : 't lval -> 't

    val decl : ('t, _, _) wtype -> 't wexpr -> 't wexpr decl

    val decl_v : 't wexpr decl -> 't wexpr lval


    val apply : ('t0 -> 't1) wfunc -> 't0 wexpr -> 't1 wexpr

    val addf : wf32 wexpr -> wf32 wexpr -> wf32 wexpr


    (* val assign : 'a wexpr lval -> 'a wexpr -> wstmt *)
  end