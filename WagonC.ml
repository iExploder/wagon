module WagonC : sig
  (* L-value tag for C *)
  type 'a lval
  (* Statement in C, 'rt: Return Type *)
  type 'rt stmt
  (* Typed Expression in C *)
  type 't expr
  (* Function in C *)
  type ('args, 'rt, 'dep) fn
  type _ fn_hl 
  type ('args, 'rt, 'dep) fn_callable = 'args expr -> 'rt expr
  type _ fn_callable_hl

  type prog

  (* Functional-Unparsing Powered C-Type Representation *)
  type ('at, 'a, 'b) ctyp

  (* Abstract Types: Primitive Types *)
  type c_void
  type c_bool

  type c_int8
  type c_int16
  type c_int32
  type c_int64

  type 'a c_ityp

  type c_float32
  type c_float64

  type 'a c_vdt

  type 'a c_struct
  type 'a c_array
  type 'a c_marg

  type 'a c_v128
  type 'a c_v256
  type 'a c_v512
  
  type (_,_) basic_accessor =
  | Begin : ('a, 'a * _) basic_accessor
  | Next  : ('a, 'r) basic_accessor -> ('a, _ * 'r) basic_accessor
  val struct_accessor : ('a, 'r) basic_accessor -> 'r c_struct expr -> 'a expr
  val marg_accessor : ('a, 'r) basic_accessor -> 'r c_marg expr -> 'a expr
  val fnc_accessor : (('a * 'b * 'c), 'r) basic_accessor -> 'r fn_callable_hl -> ('a, 'b, 'c) fn_callable 


  val c_void    : (c_void,    'a, unit  -> 'a) ctyp
  val c_bool    : (c_bool,    'a, bool  -> 'a) ctyp
  val c_int8    : (c_int8 c_ityp c_vdt,   'a, int   -> 'a) ctyp
  val c_int16   : (c_int16 c_ityp c_vdt,   'a, int   -> 'a) ctyp
  val c_int32   : (c_int32 c_ityp c_vdt,   'a, int   -> 'a) ctyp
  val c_int64   : (c_int64 c_ityp c_vdt,   'a, int   -> 'a) ctyp
  val c_float32 : (c_float32 c_vdt, 'a, float -> 'a) ctyp
  val c_float64 : (c_float64 c_vdt, 'a, float -> 'a) ctyp

  val c_string  : int -> (c_int8 c_ityp c_vdt c_array, 'a, string -> 'a) ctyp

  val make_vec128 : ('a c_vdt, _, _) ctyp -> ('a c_v128, 'b, 'a c_vdt expr array -> 'b) ctyp
  val make_vec256 : ('a c_vdt, _, _) ctyp -> ('a c_v256, 'b, 'a c_vdt expr array -> 'b) ctyp
  val make_vec512 : ('a c_vdt, _, _) ctyp -> ('a c_v512, 'b, 'a c_vdt expr array -> 'b) ctyp

  val sub_vec128 : 'a c_v128 expr -> c_int32 c_ityp c_vdt expr -> 'a c_vdt expr
  val sub_vec256 : 'a c_v256 expr -> c_int32 c_ityp c_vdt expr -> 'a c_vdt expr
  val sub_vec512 : 'a c_v512 expr -> c_int32 c_ityp c_vdt expr -> 'a c_vdt expr
  val sub_vec128l : 'a c_v128 expr -> c_int32 c_ityp c_vdt expr -> 'a c_vdt expr
  val sub_vec256l : 'a c_v256 expr -> c_int32 c_ityp c_vdt expr -> 'a c_vdt expr
  val sub_vec512l : 'a c_v512 expr -> c_int32 c_ityp c_vdt expr -> 'a c_vdt expr


  type 'a basic_mtyp
  (* Experimental Extensions *)
  val empty_mtyp : (unit basic_mtyp, 'a, 'a) ctyp

  val empty_fn : unit fn_hl
  val (@&) : ('args, 'rt, 'dep) fn -> 'any fn_hl -> (('args * 'rt * 'dep) * 'any) fn_hl
  val (@^) : ('at0, 'b, 'c) ctyp -> ('at1 basic_mtyp, 'a, 'b) ctyp -> (('at0 * 'at1) basic_mtyp, 'a, 'c) ctyp
  val make_struct : ('at basic_mtyp, 'b, 'c) ctyp -> ('at c_struct, 'b, 'c) ctyp
  val make_array : ('at, _, _) ctyp -> int -> ('at c_array, 'a, 'at expr array -> 'a) ctyp
  val make_args : ('at basic_mtyp, 'b, 'c) ctyp -> ('at c_marg, 'b, 'c) ctyp

  val expr_stmt : 't expr -> 'rt stmt
  
  val if_stmt : c_bool expr -> 'rt stmt -> 'rt stmt -> 'rt stmt
  val seq_stmt : 'rt stmt list -> 'rt stmt
  val ret_stmt : 'rt expr -> 'rt stmt
  (* For-statement generator: Notice decl_stmt is needed *)
  val for_stmt : c_bool expr * 'any expr * 'rt stmt -> 'rt stmt
  (* Constexpr initializer generator *)
  val const : ('at, 'at expr, 'b) ctyp -> 'b

  val make_fn : ?cname : string -> ('args, _, _) ctyp -> ('rt, _, _) ctyp -> 'dep fn_hl -> 
                ('dep fn_callable_hl -> 'args expr -> 'rt stmt) -> ('args, 'rt, 'dep) fn
  val make_fn_rec : ?cname : string -> ('args, _, _) ctyp -> ('rt, _, _) ctyp -> 'dep fn_hl -> 
                (('args, 'rt, 'dep) fn_callable -> 'dep fn_callable_hl -> 'args expr -> 'rt stmt) -> ('args, 'rt, 'dep) fn

  val sub_arr : ('a c_array, _, _) ctyp -> 'a c_array expr -> 'b c_ityp c_vdt expr -> 'a expr
  val sub_arrl : ('a c_array, _, _) ctyp -> 'a c_array expr lval -> 'b c_ityp c_vdt expr -> 'a expr lval
  val decl_stmt : ('at, _, _) ctyp -> 'at expr -> ('at expr lval -> 'rt stmt) -> 'rt stmt
  val delval : 'at expr lval -> 'at expr

  val assign_expr : 'at expr lval -> 'at expr -> 'at expr
  val assign_stmt : 'at expr lval -> 'at expr -> 'rt stmt

  val show_expr : 'at expr -> string
  val show_stmt : 'rt stmt -> string
  val show_func_with_dep : (_, _, _) fn -> string list
  val show_func : (_,_,_) fn -> string

  val addi : 'a c_ityp c_vdt expr * 'a c_ityp c_vdt expr -> 'a c_ityp c_vdt expr
  val subi : 'a c_ityp c_vdt expr * 'a c_ityp c_vdt expr -> 'a c_ityp c_vdt expr
  val muli : 'a c_ityp c_vdt expr * 'a c_ityp c_vdt expr -> 'a c_ityp c_vdt expr
  val divi : 'a c_ityp c_vdt expr * 'a c_ityp c_vdt expr -> 'a c_ityp c_vdt expr
  val modi : 'a c_ityp c_vdt expr * 'a c_ityp c_vdt expr -> 'a c_ityp c_vdt expr

  val addf : c_float32 c_vdt expr * c_float32 c_vdt expr -> c_float32 c_vdt expr 
  val subf : c_float32 c_vdt expr * c_float32 c_vdt expr -> c_float32 c_vdt expr 
  val mulf : c_float32 c_vdt expr * c_float32 c_vdt expr -> c_float32 c_vdt expr 
  val divf : c_float32 c_vdt expr * c_float32 c_vdt expr -> c_float32 c_vdt expr 
  val modf : c_float32 c_vdt expr * c_float32 c_vdt expr -> c_float32 c_vdt expr 

  val addd : c_float64 c_vdt expr * c_float64 c_vdt expr -> c_float64 c_vdt expr 
  val subd : c_float64 c_vdt expr * c_float64 c_vdt expr -> c_float64 c_vdt expr 
  val muld : c_float64 c_vdt expr * c_float64 c_vdt expr -> c_float64 c_vdt expr 
  val divd : c_float64 c_vdt expr * c_float64 c_vdt expr -> c_float64 c_vdt expr 
  val modd : c_float64 c_vdt expr * c_float64 c_vdt expr -> c_float64 c_vdt expr 

  val addv256i32 : c_int32 c_ityp c_v256 expr * c_int32 c_ityp c_v256 expr -> c_int32 c_ityp c_v256 expr
  val addv256f32 : c_float32 c_v256 expr * c_float32 c_v256 expr -> c_float32 c_v256 expr
  val addv256f64 : c_float64 c_v256 expr * c_float64 c_v256 expr -> c_float64 c_v256 expr
  val mulv256f32 : c_float32 c_v256 expr * c_float32 c_v256 expr -> c_float32 c_v256 expr
  val mulv256f64 : c_float64 c_v256 expr * c_float64 c_v256 expr -> c_float64 c_v256 expr
  val fmaddv256f32 : c_float32 c_v256 expr * c_float32 c_v256 expr * c_float32 c_v256 expr -> c_float32 c_v256 expr
  val fmaddv256f64 : c_float64 c_v256 expr * c_float64 c_v256 expr * c_float64 c_v256 expr -> c_float64 c_v256 expr

  val cast_v128i : 'a c_ityp c_v128 expr -> 'b c_ityp c_v128 expr
  val cast_v256i : 'a c_ityp c_v256 expr -> 'b c_ityp c_v256 expr
  val cast_v512i : 'a c_ityp c_v512 expr -> 'b c_ityp c_v512 expr

  val lei   : 'a c_ityp c_vdt expr * 'a c_ityp c_vdt expr -> c_bool expr
  val leeqi : 'a c_ityp c_vdt expr * 'a c_ityp c_vdt expr -> c_bool expr
  val eqi   : 'a c_ityp c_vdt expr * 'a c_ityp c_vdt expr -> c_bool expr
  val greqi : 'a c_ityp c_vdt expr * 'a c_ityp c_vdt expr -> c_bool expr
  val gri   : 'a c_ityp c_vdt expr * 'a c_ityp c_vdt expr -> c_bool expr

  val lef   : c_float32 c_vdt expr * c_float32 c_vdt expr -> c_bool expr
  val leeqf : c_float32 c_vdt expr * c_float32 c_vdt expr -> c_bool expr
  val eqf   : c_float32 c_vdt expr * c_float32 c_vdt expr -> c_bool expr
  val greqf : c_float32 c_vdt expr * c_float32 c_vdt expr -> c_bool expr
  val grf   : c_float32 c_vdt expr * c_float32 c_vdt expr -> c_bool expr

  val led   : c_float64 c_vdt expr * c_float64 c_vdt expr -> c_bool expr
  val leeqd : c_float64 c_vdt expr * c_float64 c_vdt expr -> c_bool expr
  val eqd   : c_float64 c_vdt expr * c_float64 c_vdt expr -> c_bool expr
  val greqd : c_float64 c_vdt expr * c_float64 c_vdt expr -> c_bool expr
  val grd   : c_float64 c_vdt expr * c_float64 c_vdt expr -> c_bool expr

  val make_prog : (c_int64 c_ityp c_vdt * (c_int8 c_ityp c_vdt c_array c_array * unit) c_marg, c_int64 c_ityp c_vdt, 'a) fn -> prog
  val make_prog_simple : (c_void, c_int64 c_ityp c_vdt, _) fn -> prog
  val output_prog : prog -> string

end 
= struct 
  open Str
  (* L-value tag for C *)
  type 'a lval = 'a
  (* Statement in C, 'rt: Return Type *)
  type 'rt stmt = string
  (* Typed Expression in C *)
  type 't expr = string
  (* Declaration in C *)
  type 'a decl = {dl_name : string; dl_stmt : 'b. 'b stmt}

  type ('args, 'rt, 'dep) fn = {fn_name : string; fn_decl : string ; fn_dep : 'dep fn_hl}
  and _ fn_hl = 
  | Nodep : unit fn_hl
  | Lnk : ('args, 'rt, 'dep) fn * 'base fn_hl -> (('args * 'rt * 'dep) * 'base) fn_hl

  type ('args, 'rt, 'dep) fn_callable = 'args expr -> 'rt expr
  type _ fn_callable_hl =
  | NoneFnCHL : unit fn_callable_hl
  | ConsFnCHL : ('args, 'rt, 'dep) fn_callable * 'any fn_callable_hl -> 
                (('args * 'rt * 'dep) * 'any) fn_callable_hl

  

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

  type c_int8
  type c_int16
  type c_int32
  type c_int64

  type 'a c_ityp
  type c_float32
  type c_float64

  type 'a c_vdt

  type 'a c_struct
  type 'a c_array
  type 'a c_marg

  type 'a c_v128
  type 'a c_v256
  type 'a c_v512

  type 'a basic_mtyp

  type prog = { 
    incl : string list; 
    func : string list
  }

  type (_,_) basic_accessor =
  | Begin : ('a, 'a * _) basic_accessor
  | Next  : ('a, 'r) basic_accessor -> ('a, _ * 'r) basic_accessor

  let rec get_accessor_index : type a r . (a, r) basic_accessor -> int = function
  | Begin -> 0
  | Next n -> 1 + get_accessor_index n 
  let struct_accessor =
  fun n l ->
    Printf.sprintf "(%s).m%d" l (get_accessor_index n)

  let marg_accessor =
  fun n l ->
    Printf.sprintf "p%d" (get_accessor_index n)
  
  let rec fnc_accessor : type a b c r. ((a * b * c), r) basic_accessor -> r fn_callable_hl -> (a,b,c) fn_callable = fun ptr term ->
  match (ptr,term) with
  | (Begin, ConsFnCHL(f,_)) -> f
  | (Next n, ConsFnCHL(_,l)) -> fnc_accessor n l

  let c_void = {
    decl = (fun x -> "");
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
  let c_int8 = {
    decl = (fun x -> "int8_t " ^ x);
    tn = "int8_t";
    init = (fun f x -> f (Printf.sprintf "(int8_t)%d" x));
    size = 1;
    abbr = fun () -> "i"
  }
  let c_int16 = {
    decl = (fun x -> "int16_t " ^ x);
    tn = "int16_t";
    init = (fun f x -> f (string_of_int x));
    size = 2;
    abbr = fun () -> "i"
  }
  let c_int32 = {
    decl = (fun x -> "int32_t " ^ x);
    tn = "int32_t";
    init = (fun f x -> f (string_of_int x));
    size = 4;
    abbr = fun () -> "i"
  }
  let c_int64 = {
    decl = (fun x -> "int64_t " ^ x);
    tn = "int64_t";
    init = (fun f x -> f (string_of_int x));
    size = 8;
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
    size = 8;
    abbr = fun () -> "d"
  }

  let c_string l = {
    decl = (fun x -> Printf.sprintf "char %s[%d]" x l);
    tn = "char*";
    init = (fun f x -> if String.length x > l-1 then failwith "oversized string literal" 
                       else f ("\"" ^ String.escaped x ^ "\""));
    size = l;
    abbr = fun () -> failwith "Should not happen"
  }

  let empty_mtyp = {
    decl = (fun x -> "");
    tn = "<UNIT>";
    init = (fun f -> f "");
    size = 0;
    abbr = fun () -> failwith "Should not happen"
  }

  let make_vec128 vdt = 
  let vname = Printf.sprintf "__m128%s" (vdt.abbr ()) in 
  let vlen = 16 in
  {
    decl = (fun x -> Printf.sprintf "%s %s" vname x);
    tn = vname;
    init = (fun f x -> if Array.length x = (vlen / vdt.size) 
                       then f (Printf.sprintf "{%s}" (String.concat ", " @@ Array.to_list x))
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
                       then f (Printf.sprintf "{%s}" (String.concat ", " @@ Array.to_list x))
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
                       then f (Printf.sprintf "{%s}" (String.concat ", " @@ Array.to_list x))
                       else failwith "vec512 initializer size mismatch");
    size = vlen;
    abbr = fun () -> failwith "Should not happen"
  }

  let (@^) = fun x y -> {
    decl = (fun a -> x.decl @@ y.decl a);
    (* Check if EMPTY_MTYP terminator *)
    tn = if y.tn != empty_mtyp.tn then x.tn ^ "," ^ y.tn else x.tn;
    init = (fun k -> x.init (fun sa -> y.init (
      if y.tn != empty_mtyp.tn then (fun sb -> k (sa ^ ", " ^ sb))
      else (fun sb -> k (sa ^ sb)))))
    ;
    size = x.size + y.size;
    abbr = fun () -> failwith "Should not happen"
  }
  let _lbk = fun k -> k "{"
  let _rbk = fun k -> k "}"
  let _con x y = fun k -> x (fun sa -> y (fun sb -> k (sa ^ sb)))
  let make_struct = fun x -> 
    let struct_name = Printf.sprintf "ws_%08x" @@ Hashtbl.hash (x.decl "") in
    let struct_name_fmt = fun k -> k (Printf.sprintf "(%s)" struct_name) in
    {
        decl = (fun a -> struct_name ^ " " ^ a);
        tn = struct_name;
        init = _con struct_name_fmt(_con (_con _lbk x.init) _rbk);
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


  let rec fn_hl_transform : type a. a fn_hl -> a fn_callable_hl =
  fun fnhl ->
  let fn_transform fn = fun args -> Printf.sprintf "%s(%s)" fn.fn_name args in
  match fnhl with
  | Nodep -> NoneFnCHL
  | Lnk(a,b) -> ConsFnCHL (fn_transform a, fn_hl_transform b)


  let make_fn_rec ?cname argt rt dep rawf =
    let check_name s = string_match (regexp "[_a-zA-Z][_a-zA-Z0-9]*") s 0 in
    let fn_name = 
      match cname with
      | None -> Printf.sprintf "wf_%08x" @@ Hashtbl.hash rawf 
      | Some n -> if check_name n then Printf.sprintf "%s_%08x" n @@ Hashtbl.hash rawf
                  else failwith "Unexpected C function name"
    in
    let self_fn x = Printf.sprintf "%s(%s)" fn_name x in
    {
      fn_name = fn_name;
      fn_decl = Printf.sprintf "%s %s(%s)\n{\n%s\n}" 
        rt.tn fn_name (argt.decl "p0") (rawf self_fn (fn_hl_transform dep) "p0");
      fn_dep = dep
    }
  let make_fn ?cname argt rt dep rawf =
    match cname with
    | None -> make_fn_rec argt rt dep (fun _ -> rawf) 
    | Some cn -> make_fn_rec ~cname:cn argt rt dep (fun _ -> rawf)

  let empty_fn = Nodep
  let (@&) a b = Lnk(a,b)
  
    
  let ret_stmt = fun x -> Printf.sprintf "return %s;" x

  let if_stmt = fun cond t f -> Printf.sprintf "if (%s) { %s } else { %s }" cond t f

  let seq_stmt = fun l -> List.fold_left (^) "" l

  let expr_stmt = fun x -> x ^ ";"


  let const = fun x -> x.init (fun a -> a)

  let sub_arr typ v n = Printf.sprintf "%s[%s]" v n
  let sub_arrl = sub_arr

  let sub_vec128 = sub_arr ()
  let sub_vec256 = sub_arr ()
  let sub_vec512 = sub_arr ()

  let sub_vec128l = sub_arr ()
  let sub_vec256l = sub_arr ()
  let sub_vec512l = sub_arr ()


  let _decl_counter = ref 0
  let _decl_get_counter () = let n = !_decl_counter in _decl_counter := !_decl_counter + 1 ; n

  let decl_stmt typ iv fxs = 
    let decl_name = Printf.sprintf "wdecl_%d" @@ _decl_get_counter () in
    Printf.sprintf "{ %s = %s; %s }" (typ.decl decl_name) iv (fxs decl_name)

  let for_stmt : c_bool expr * 'any expr * 'rt stmt -> 'rt stmt =
  fun (cond,step,s) -> 
    Printf.sprintf "for (;%s;%s) {%s}" cond step s
  let delval = fun x -> x

  let assign_expr lhs rhs = Printf.sprintf "%s = %s" lhs rhs
  let assign_stmt lhs rhs = Printf.sprintf "%s = %s;" lhs rhs

  let show_expr = fun x -> x
  let show_stmt = fun x -> x
  let rec show_func_with_dep_inner : type args rt dep. (args, rt, dep) fn -> string list = fun fn -> 
    let dependencies = (show_fn_dep fn.fn_dep) in 
    dependencies @ [fn.fn_decl]
  and show_fn_dep : type a. a fn_hl -> string list = function
  | Nodep -> []
  | Lnk (hd, tl) -> (show_func_with_dep_inner hd) @ (show_fn_dep tl)

  let uniq_cons x xs = if List.mem x xs then xs else x :: xs

  let remove_from_right xs = List.fold_right uniq_cons xs []
  let show_func_with_dep f = remove_from_right @@ show_func_with_dep_inner f

  let show_func f = f.fn_decl


  (** Pervasives and Built-in Vector Instructions *)
  type _wp = {
    add : string * string -> string;
    sub : string * string -> string;
    mul : string * string -> string;
    div : string * string -> string;
    modulo : string * string -> string;
    gr : string * string -> string;
    greq : string * string -> string;
    eq : string * string -> string;
    leeq : string * string -> string;
    le : string * string -> string;
  }
  let _wp = {
    add = (fun (x,y) -> Printf.sprintf "%s + %s" x y);
    sub = (fun (x,y) -> Printf.sprintf "%s - %s" x y);
    mul = (fun (x,y) -> Printf.sprintf "%s * %s" x y);
    div = (fun (x,y) -> Printf.sprintf "%s / %s" x y);
    modulo = (fun (x,y) -> Printf.sprintf "%s %% %s" x y);
    gr   = (fun (x,y) -> Printf.sprintf "%s > %s" x y);
    greq = (fun (x,y) -> Printf.sprintf "%s >= %s" x y);
    eq   = (fun (x,y) -> Printf.sprintf "%s == %s" x y);
    leeq = (fun (x,y) -> Printf.sprintf "%s <= %s" x y);
    le   = (fun (x,y) -> Printf.sprintf "%s < %s" x y)
  }
  let addi = _wp.add
  let subi = _wp.sub
  let muli = _wp.mul
  let divi = _wp.div
  let modi = _wp.modulo

  let addf = _wp.add
  let subf = _wp.sub
  let mulf = _wp.mul
  let divf = _wp.div
  let modf = fun (x,y) -> Printf.sprintf "fmod(%s, %s)" x y

  let addd = _wp.add
  let subd = _wp.sub
  let muld = _wp.mul
  let divd = _wp.div
  let modd = fun (x,y) -> Printf.sprintf "fmod(%s, %s)" x y

  let gri = _wp.gr
  let greqi = _wp.greq
  let eqi = _wp.eq
  let leeqi = _wp.leeq
  let lei = _wp.le
  let grf = _wp.gr
  let greqf = _wp.greq
  let eqf = _wp.eq
  let leeqf = _wp.leeq
  let lef = _wp.le
  let grd = _wp.gr
  let greqd = _wp.greq
  let eqd = _wp.eq
  let leeqd = _wp.leeq
  let led = _wp.le

  let addv256i32 = fun (x,y) -> Printf.sprintf "_mm256_add_epi32(%s, %s)" x y
  let addv256f32 = fun (x,y) -> Printf.sprintf "_mm256_add_ps(%s, %s)" x y
  let addv256f64 = fun (x,y) -> Printf.sprintf "_mm256_add_pd(%s, %s)" x y
  let mulv256f32 = fun (x,y) -> Printf.sprintf "_mm256_mul_ps(%s, %s)" x y
  let mulv256f64 = fun (x,y) -> Printf.sprintf "_mm256_mul_pd(%s, %s)" x y
  let fmaddv256f32 = fun (x,y,z) -> Printf.sprintf "_mm256_fmadd_ps(%s, %s, %s)" x y z
  let fmaddv256f64 = fun (x,y,z) -> Printf.sprintf "_mm256_fmadd_pd(%s, %s, %s)" x y z

  let cast_v128i = fun x -> x
  let cast_v256i = fun x -> x
  let cast_v512i = fun x -> x

  let make_prog fn = {
    incl = ["stdint.h"; "math.h"; "immintrin.h"]; 
    func = show_func_with_dep fn @ [Printf.sprintf "int main(int argc, char** argv) \n{\n\treturn %s(argc, argv); \n}" fn.fn_name]
  }
  let make_prog_simple fn = {
    incl = ["stdint.h"; "math.h"; "immintrin.h"]; 
    func = show_func_with_dep fn @ [Printf.sprintf "int main() \n{\n\treturn %s(); \n}" fn.fn_name]
  }

  let output_prog p = 
    let include_headers = 
      List.map (fun x -> Printf.sprintf "#include <%s>" x) p.incl |> (String.concat "\n") 
    in
    let full_code = p.func |> String.concat "\n" in
    include_headers ^ "\n" ^ full_code
    


end
