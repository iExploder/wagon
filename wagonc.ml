module type WagonC = sig
  (* L-value tag for C *)
  type 'a lval
  (* Statement in C, 'rt: Return Type *)
  type 'rt stmt
  (* Typed Expression in C *)
  type 't expr
  (* Declaration in C *)
  type 'a decl
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

  type 'a c_vdt

  type 'a c_struct
  type 'a c_array
  type 'a c_marg

  type 'a c_v128
  type 'a c_v256
  type 'a c_v512
  
  type arg2_accessor = {
    _0 : 'a 'b. ('a * 'b) c_marg expr -> 'a expr;
    _1 : 'a 'b. ('a * 'b) c_marg expr -> 'b expr;
  }
  type arg3_accessor = {
    _0 : 'a 'b 'c. ('a * ('b * 'c)) c_marg expr -> 'a expr;
    _1 : 'a 'b 'c. ('a * ('b * 'c)) c_marg expr -> 'b expr;
    _2 : 'a 'b 'c. ('a * ('b * 'c)) c_marg expr -> 'c expr;
  }

  type str2_accessor = {
    _0 : 'a 'b. ('a * 'b) c_struct expr -> 'a expr;
    _1 : 'a 'b. ('a * 'b) c_struct expr -> 'b expr;
  }
  type str3_accessor = {
    _0 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr -> 'a expr;
    _1 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr -> 'b expr;
    _2 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr -> 'c expr;
  }

  type str2l_accessor = {
    _0 : 'a 'b. ('a * 'b) c_struct expr lval -> 'a expr lval;
    _1 : 'a 'b. ('a * 'b) c_struct expr lval -> 'b expr lval;
  }
  type str3l_accessor = {
    _0 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr lval -> 'a expr lval;
    _1 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr lval -> 'b expr lval;
    _2 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr lval -> 'c expr lval;
  }

  val c_void    : (c_void,    'a, unit  -> 'a) ctyp
  val c_bool    : (c_bool,    'a, bool  -> 'a) ctyp
  val c_int32   : (c_int32 c_vdt,   'a, int   -> 'a) ctyp
  val c_float32 : (c_float32 c_vdt, 'a, float -> 'a) ctyp
  val c_float64 : (c_float64 c_vdt, 'a, float -> 'a) ctyp

  val make_vec128 : ('a c_vdt, _, _) ctyp -> ('a c_v128, 'b, 'a c_vdt expr array -> 'b) ctyp
  val make_vec256 : ('a c_vdt, _, _) ctyp -> ('a c_v256, 'b, 'a c_vdt expr array -> 'b) ctyp
  val make_vec512 : ('a c_vdt, _, _) ctyp -> ('a c_v512, 'b, 'a c_vdt expr array -> 'b) ctyp

  val sub_vec128 : 'a c_v128 expr -> c_int32 c_vdt expr -> 'a c_vdt expr
  val sub_vec256 : 'a c_v256 expr -> c_int32 c_vdt expr -> 'a c_vdt expr
  val sub_vec512 : 'a c_v512 expr -> c_int32 c_vdt expr -> 'a c_vdt expr


  val (@+) : ('at0, 'b, 'c) ctyp -> ('at1, 'a, 'b) ctyp -> ('at0 * 'at1, 'a, 'c) ctyp
  val make_struct : ('at0 * 'at1, 'b, 'c) ctyp -> (('at0 * 'at1) c_struct, 'b, 'c) ctyp
  val make_array : ('at, _, _) ctyp -> int -> ('at c_array, 'a, 'at expr array -> 'a) ctyp
  val make_args : ('at0 * 'at1, 'b, 'c) ctyp -> (('at0 * 'at1) c_marg, 'b, 'c) ctyp

  val expr_stmt : 't expr -> 'rt stmt
  
  val if_stmt : c_bool expr -> 'rt stmt -> 'rt stmt -> 'rt stmt
  val seq_stmt : 'rt stmt list -> 'rt stmt
  val ret_stmt : 'rt expr -> 'rt stmt
  (* Constexpr initializer generator *)
  val const : ('at, 'at expr, 'b) ctyp -> 'b

  val make_fn : ('args, _, _) ctyp -> ('rt, _, _) ctyp -> ('args expr -> 'rt stmt) -> ('args, 'rt) fn
  val call_fn : ('args, _, _) ctyp -> ('rt, _, _) ctyp -> ('args, 'rt) fn -> 'args expr -> 'rt expr

  val sub_arg2 : arg2_accessor
  val sub_arg3 : arg3_accessor
  val sub_str2 : str2_accessor
  val sub_str3 : str3_accessor
  val sub_str2l : str2l_accessor
  val sub_str3l : str3l_accessor

  val sub_arr : ('a c_array, _, _) ctyp -> 'a c_array expr -> c_int32 c_vdt expr -> 'a expr
  val sub_arrl : ('a c_array, _, _) ctyp -> 'a c_array expr lval -> c_int32 c_vdt expr -> 'a expr lval
  val decl : ('at, _, _) ctyp -> 'at expr -> 'at decl

  val decl_stmt : 'at decl -> 'b stmt
  val decl_exprl : 'at decl -> 'at expr lval

  val assign_stmt : 'at expr lval -> 'at expr -> 'rt stmt

  val show_expr : 'at expr -> string
  val show_stmt : 'rt stmt -> string
  val show_func : (_,_) fn -> string

  val addi32 : c_int32 c_vdt expr -> c_int32 c_vdt expr -> c_int32 c_vdt expr
  val addv256i32 : ((c_int32 c_v256 * c_int32 c_v256) c_marg, c_int32 c_v256) fn
  val addv256i32_e : c_int32 c_v256 expr -> c_int32 c_v256 expr -> c_int32 c_v256 expr
  
end

module WagonC_TF : WagonC = struct 
  (* L-value tag for C *)
  type 'a lval = 'a
  (* Statement in C, 'rt: Return Type *)
  type 'rt stmt = string
  (* Typed Expression in C *)
  type 't expr = string
  (* Declaration in C *)
  type 'a decl = {dl_name : string; dl_stmt : 'b. 'b stmt}
  (* Function in C *)
  type ('args, 'rt) fn = { fn_name : string; fn_decl : string }

  (* Functional-Unparsing Powered C-Type Representation *)
  type ('at, 'a, 'b) ctyp = {
    decl : string -> string;
    tn : string;
    init : (string -> 'a) -> 'b;
    size : int;
    abbr : unit -> string
  }

  (* Abstract Types: Primitive Types *)
  type c_void
  type c_bool
  type c_int32
  type c_float32
  type c_float64

  type 'a c_vdt

  type 'a c_struct
  type 'a c_array
  type 'a c_marg

  type 'a c_v128
  type 'a c_v256
  type 'a c_v512

  type arg2_accessor = {
    _0 : 'a 'b. ('a * 'b) c_marg expr -> 'a expr;
    _1 : 'a 'b. ('a * 'b) c_marg expr -> 'b expr;
  }
  type arg3_accessor = {
    _0 : 'a 'b. ('a * 'b) c_marg expr -> 'a expr;
    _1 : 'a 'b 'c. ('a * ('b * 'c)) c_marg expr -> 'b expr;
    _2 : 'a 'b 'c. ('a * ('b * 'c)) c_marg expr -> 'c expr;
  }

  type str2_accessor = {
    _0 : 'a 'b. ('a * 'b) c_struct expr -> 'a expr;
    _1 : 'a 'b. ('a * 'b) c_struct expr -> 'b expr;    
  }
  type str3_accessor = {
    _0 : 'a 'b. ('a * 'b) c_struct expr -> 'a expr;
    _1 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr -> 'b expr;
    _2 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr -> 'c expr;
  }
  type str2l_accessor = {
    _0 : 'a 'b. ('a * 'b) c_struct expr lval -> 'a expr lval;
    _1 : 'a 'b. ('a * 'b) c_struct expr lval -> 'b expr lval;
  }
  type str3l_accessor = {
    _0 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr lval -> 'a expr lval;
    _1 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr lval -> 'b expr lval;
    _2 : 'a 'b 'c. ('a * ('b * 'c)) c_struct expr lval -> 'c expr lval;
  }

  let c_void = {
    decl = (fun x -> "void " ^ x);
    tn = "void";
    init = (fun f () -> f "()");
    size = 0;
    abbr = fun () -> failwith "Should not happen"
  }
  let c_bool = {
    decl = (fun x -> "bool " ^ x);
    tn = "bool";
    init = (fun f x -> f (if x then "true" else "false"));
    size = 1;
    abbr = fun () -> failwith "Should not happen"
  }
  let c_int32 = {
    decl = (fun x -> "int32_t " ^ x);
    tn = "int32";
    init = (fun f x -> f (string_of_int x));
    size = 4;
    abbr = fun () -> "i"
  }
  let c_float32 = {
    decl = (fun x -> "float " ^ x);
    tn = "float";
    init = (fun f x -> f (string_of_float x ^ "f"));
    size = 4;
    abbr = fun () -> ""
  }
  let c_float64 = {
    decl = (fun x -> "double " ^ x);
    tn = "double";
    init = (fun f x -> f (string_of_float x));
    size = 4;
    abbr = fun () -> "d"
  }
  let make_vec128 vdt = 
  let vname = Printf.sprintf "__m128%s" (vdt.abbr ()) in 
  let vlen = 16 in
  {
    decl = (fun x -> Printf.sprintf "%s %s" vname x);
    tn = vname;
    init = (fun f x -> if Array.length x = (vlen / vdt.size) 
                       then f (String.concat ", " x)
                       else failwith "vec128 initializer size mismatch");
    size = vlen;
    abbr = fun () -> failwith "Should not happen"
  }
  let make_vec256 vdt = 
  let vname = Printf.sprintf "__m256%s" (vdt.abbr ()) in 
  let vlen = 32 in
  {
    decl = (fun x -> Printf.sprintf "%s %s" vname x);
    tn = vname;
    init = (fun f x -> if Array.length x = (vlen / vdt.size) 
                       then f (String.concat ", " x)
                       else failwith "vec256 initializer size mismatch");
    size = vlen;
    abbr = fun () -> failwith "Should not happen"
  }
  let make_vec512 vdt = 
  let vname = Printf.sprintf "__m512%s" (vdt.abbr ()) in 
  let vlen = 64 in
  {
    decl = (fun x -> Printf.sprintf "%s %s" vname x);
    tn = vname;
    init = (fun f x -> if Array.length x = (vlen / vdt.size) 
                       then f (String.concat ", " x)
                       else failwith "vec512 initializer size mismatch");
    size = vlen;
    abbr = fun () -> failwith "Should not happen"
  }

  let (@+) = fun x y -> {
    decl = (fun a -> x.decl @@ y.decl a);
    tn = x.tn ^ "," ^ y.tn;
    init = (fun k -> x.init (fun sa -> y.init (fun sb -> k (sa ^ ", " ^ sb))));
    size = x.size + y.size;
    abbr = fun () -> failwith "Should not happen"
  }
  let _lbk = fun k -> k "{"
  let _rbk = fun k -> k "}"
  let _con x y = fun k -> x (fun sa -> y (fun sb -> k (sa ^ sb)))
  let make_struct = fun x -> 
    let struct_name = Printf.sprintf "ws_%08x" @@ Hashtbl.hash (x.decl "") in
    {
        decl = (fun a -> struct_name ^ " " ^ a);
        tn = struct_name;
        init = _con (_con _lbk x.init) _rbk;
        size = x.size;
        abbr = fun () -> failwith "Should not happen"
    }
  let make_array = fun t n -> {
    decl = (fun x -> Printf.sprintf "%s[%d]" (t.decl x) n);
    tn = Printf.sprintf "%s[%d]" t.tn n;
    init = (fun k arr -> 
      if Array.length arr > n then failwith ""
      else
      let lst = Array.to_list arr in
      k ( "{" ^ (String.concat ", " lst) ^ "}"));
    size = n * t.size;
    abbr = fun () -> failwith "Should not happen"
  }

  let make_args = fun x -> 
    let args_list = String.split_on_char ',' x.tn in
    (* TODO: Replace [] into "*" *)
    {
        decl = (fun _ -> 
          String.concat ", " @@ 
            List.mapi (fun i n -> 
              Printf.sprintf "%s p%d" n i) args_list);
        tn = x.tn;
        init = x.init;
        size = -1;
        abbr = fun () -> failwith "Should not happen"
    }

  let make_fn argt rt = fun rawf -> 
    let fn_name = Printf.sprintf "wf_%08x" @@ Hashtbl.hash rawf in
    {
      fn_name = fn_name;
      fn_decl = Printf.sprintf "%s %s(%s)\n{\n%s\n}" 
        rt.tn fn_name (argt.decl "p0") (rawf "p0")
    }
  
  let call_fn argt rt fx args = Printf.sprintf "%s(%s)" fx.fn_name args
    
  let ret_stmt = fun x -> Printf.sprintf "return %s;" x

  let if_stmt = fun cond t f -> Printf.sprintf "if (%s) { %s } else { %s }" cond t f

  let seq_stmt = fun l -> List.fold_left (^) "" l

  let expr_stmt = fun x -> x ^ ";"


  let const = fun x -> x.init (fun a -> a)

  let sub_arg2 : arg2_accessor = {
    _0 = (fun _ -> "p0");
    _1 = (fun _ -> "p1")
  }

  let sub_arg3 : arg3_accessor = {
    _0 = (fun _ -> "p0");
    _1 = (fun _ -> "p1");
    _2 = (fun _ -> "p2")
  }

  let sub_str2 : str2_accessor = {
    _0 = (fun a -> a ^ ".m0");
    _1 = (fun a -> a ^ ".m1")
  }

  let sub_str3 : str3_accessor = {
    _0 = (fun a -> a ^ ".m0");
    _1 = (fun a -> a ^ ".m1");
    _2 = (fun a -> a ^ ".m2")
  }


  let sub_str2l : str2l_accessor = {
    _0 = (fun a -> a ^ ".m0");
    _1 = (fun a -> a ^ ".m1")
  }

  let sub_str3l : str3l_accessor = {
    _0 = (fun a -> a ^ ".m0");
    _1 = (fun a -> a ^ ".m1");
    _2 = (fun a -> a ^ ".m2")
  }

  let sub_arr typ v n = Printf.sprintf "%s[%s]" v n
  let sub_arrl = sub_arr

  let sub_vec128 = sub_arr ()
  let sub_vec256 = sub_arr ()
  let sub_vec512 = sub_arr ()


  let _decl_counter = ref 0
  let _decl_get_counter () = let n = !_decl_counter in _decl_counter := !_decl_counter + 1 ; n
  let decl typ iv = 
    let decl_name = Printf.sprintf "wdecl_%d" @@ _decl_get_counter () in
    {
      dl_name = decl_name;
      dl_stmt = Printf.sprintf "%s = %s;" (typ.decl decl_name) iv
    }

  let decl_stmt d = d.dl_stmt

  let decl_exprl d = d.dl_name
  let assign_stmt lhs rhs = Printf.sprintf "%s = %s;" lhs rhs

  let show_expr = fun x -> x
  let show_stmt = fun x -> x
  let show_func = fun x -> x.fn_decl

  let addi32 = fun x y -> Printf.sprintf "%s + %s" x y

  let addv256i32 = {
    fn_name = "_mm256_add_epi32";
    fn_decl = ""
  }
  let addv256f32 = {
    fn_name = "_mm256_add_ps";
    fn_decl = ""
  }

  let addv256i32_e = fun x y -> Printf.sprintf "_mm256_add_epi32(%s, %s)" x y
  let addv256f64 = {
    fn_name = "_mm256_add_pd";
    fn_decl = ""
  }
end