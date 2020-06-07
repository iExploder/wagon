module WagonGeneral : WagonBasic = struct
  open Printf
  type 'a wexpr = string
  type ('a, 'b) wfus = (string -> 'a) -> 'b
  type ('at, 'wfa, 'wfb) wtype = {
    name : string;
    init : ('wfa, 'wfb) wfus;
  }
  type wstmt = string

  (* L-value in C *)
  type 't lval = 't

  type 't decl = {v : 't lval; decl : wstmt}
  type 't wfunc = {f : 't wexpr; decl : wstmt}

  let cons_finit_in = fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ ", " ^ sb)))

  let cons_wtype :
    ('at0, 'wfb, 'wfc) wtype ->
    ('at1, 'wfa, 'wfb) wtype -> ('at0 * 'at1, 'wfa, 'wfc) wtype =
    fun a b -> {
      name = sprintf "%s, %s" a.name b.name;
      init = cons_finit_in a.init b.init
    }

  (* Abstract Types *)
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

  let wbln  : (wbln ityp, 'a, bool -> 'a) wtype =
  {name = "bool";    init = fun k v -> k (if v then "true" else "false")}
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
  let lit = fun x -> x.init (fun a -> a)

  let warray = fun st i -> {
    name = sprintf "%s[%d]" st.name i;
    init = fun k arr -> 
      let lst = Array.to_list arr in
      k ("{" ^ String.concat ", " lst ^ "}")
  }

  let _lbk  : ('a, 'a) wfus = fun k -> k "{"
  let _rbk  : ('a, 'a) wfus = fun k -> k "}"
  let _cons_none : ('b, 'c) wfus -> ('a, 'b) wfus -> ('a, 'c) wfus =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ sb)))

  let wstruct = fun t -> 
    let typehash = Hashtbl.hash t.name in
    {
      name = sprintf "ws_%08x" typehash;
      init = (_cons_none (_cons_none _lbk t.init) _rbk)
    }
  let show = fun x -> x

  let delval = fun x -> x

  let _decl_counter = ref 0
  let _get_counter = 
    fun () -> 
      let counter = !_decl_counter in 
      _decl_counter := (!_decl_counter) + 1;
      counter
  let decl = fun t i -> 
  let counter = _get_counter () in
  {
    v = sprintf "wv_%08x" counter;
    decl = sprintf "%s wv_%08x = %s;" t.name counter i
  }


  let make_func = 
  fun t0 t1 e -> 
  let f_index = _get_counter () in
  {
    f = sprintf "wf_%08x" f_index;
    (* How to get typename? *)
    decl = sprintf "%s wf_%08x(%s param) { return %s; }"
      t1.name f_index t0.name (e "param");
  }
  let apply = 
  fun f x ->
    sprintf "%s(%s)" f.f x
  let addf = fun a b -> a ^ "+" ^ b
  
end