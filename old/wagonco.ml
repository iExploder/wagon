
module type WagonC = sig
  (* Statement in C, 'rt: Return Type *)
  type 'rt stmt
  (* Typed Expression in C *)
  type 't expr
  (* Function in C *)
  type ('args, 'rt) fn

  (* Functional-Unparsing Powered C-Type Representation *)
  type ('at, 'a, 'b) ctyp

  (* Abstract Types: Primitive Types *)
  type c_void
  type c_bool
  type c_int32
  type c_float32
  type c_float64

  type 'a c_struct
  type 'a c_array

  val c_void    : (c_void,    'a, unit  -> 'a) ctyp
  val c_bool    : (c_bool,    'a, bool  -> 'a) ctyp
  val c_int32   : (c_int32,   'a, int   -> 'a) ctyp
  val c_float32 : (c_float32, 'a, float -> 'a) ctyp
  val c_float64 : (c_float64, 'a, float -> 'a) ctyp
  
  val (@+) : ('at0, 'b, 'c) ctyp -> ('at1, 'a, 'b) ctyp -> ('at0 * 'at1, 'a, 'c) ctyp

  val make_struct : ('at0 * 'at1, 'b, 'c) ctyp -> (('at0 * 'at1) c_struct, 'b, 'c) ctyp
  val make_array : ('at, _, _) ctyp -> int -> ('at c_array, 'a, 'at expr array -> 'a) ctyp

  val expr_stmt : 't expr -> 'rt stmt
  
  val if_stmt : c_bool expr -> 'rt stmt -> 'rt stmt -> 'rt stmt
  val seq_stmt : 'rt stmt list -> 'rt stmt
  val ret_stmt : 'rt expr -> 'rt stmt

  (* Constexpr initializer generator *)
  val const : ('at, 'at expr, 'b) ctyp -> 'b

  val make_fn : ('args, _, _) ctyp -> ('rt, _, _) ctyp -> ('args expr -> 'rt stmt) -> ('args, 'rt) fn

  val call_fn : ('args, _, _) ctyp -> ('rt, _, _) ctyp -> ('args, 'rt) fn -> 'args expr -> 'rt expr

  



end

module WagonC_TF : WagonC = struct 
  (* Statement in C, 'rt: Return Type *)
  type 'rt stmt = string
  (* Typed Expression in C *)
  type 't expr = string
  (* Function in C *)
  type ('args, 'rt) fn = { fn_name : string; fn_decl : string; }

  (* Functional-Unparsing Powered C-Type Representation *)
  type ('at, 'a, 'b) ctyp = {
    decl : string -> string;
    typ_name : string;
    init : (string -> 'a) -> 'b
  }

  (* Abstract Types: Primitive Types *)
  type c_void
  type c_bool
  type c_int32
  type c_float32
  type c_float64

  type 'a c_struct
  type 'a c_array

  let c_void : (c_void, 'a, unit -> 'a) ctyp = {
    decl = fun x -> "void " ^ x;
    typ_name = "void";
    init = fun f x -> f "()"
  }
  let c_bool = {
    decl = fun x -> "bool " ^ x;
    typ_name = "bool";
    init = fun f x -> f (if x then "true" else "false")
  }
  let c_int32 = {
    decl = fun x -> "int32_t " ^ x;
    typ_name = "int32";
    init = fun f x -> f (string_of_int x)
  }
  let c_float32 = {
    decl = fun x -> "float " ^ x;
    typ_name = "float";
    init = fun f x -> f (string_of_float x ^ "f")
  }
  let c_float64 = {
    decl = fun x -> "double " ^ x;
    typ_name = "double";
    init = fun f x -> f (string_of_float x)
  }

  let (@+) = fun x y -> {
    decl = fun a -> x.decl @@ y.decl a;
    typ_name = x.typ_name ^ "," ^ y.typ_name;
    init = fun k -> x.init (fun sa -> y.init (fun sb -> k (sa ^ ", " ^ sb)))
  }
  
  let _lbk = fun k -> k "{"
  let _rbk = fun k -> k "}"
  let _con x y = fun k -> x (fun sa -> y (fun sb -> k (sa ^ sb)))
  let make_struct = fun x -> 
    let struct_name = Printf.sprintf "ws_%08x" @@ Hashtbl.hash (x.decl "") in
    {
        decl = fun a -> struct_name ^ " " ^ a;
        typ_name = struct_name;
        init = _con _lbk (_con (x.init _rbk))
    }
  let make_array = fun t n -> {
    decl = fun x -> Printf.sprintf "%s[%d]" (t.decl x) n;
    typ_name = Printf.sprintf "%s[%d]" t.typ_name n;
    init = fun k arr -> 
      if Array.length > n then failwith ""
      let lst = Array.to_list arr in
      k ("{" ^ String.concat ", " lst ^ "}")
  }
  
  let ret_stmt = fun x -> Printf.sprintf "return %s;" x

  let if_stmt = fun cond t f -> Printf.sprintf "if (%s) { %s } else { %s }" cond t f

  let seq_stmt = fun l -> List.fold_left (^) "" l

  let expr_stmt = fun x -> x ^ ";"

  let make_fn : ('args, _, _) ctyp -> ('rt, _, _) ctyp -> ('args expr -> 'rt stmt) -> ('args, 'rt) fn
  = fun pt rt fx ->
  let splitted_types = String.split_on_char ',' pt.typ_name in
  Printf.sprintf "%s wf_%08x(%s) { %s }" rt.typ_name (Hashtbl.hash fx) pt.type

  let call_fn = fun

  let const = fun x -> x.init (fun a -> a)
end