open WagonC
open WagonC
open MTypeAccessor
open MTypeAccessor

(* Example Function *)

let func0 = make_fn c_int32 c_int32 empty_fn (fun _ a -> ret_stmt @@ addi (a, (const c_int32 1)))
let func0_str = show_func_with_dep func0

let func1 = make_fn ~cname:"myfunc1" c_int32 c_int32 (func0 @& empty_fn) 
  (fun fd a -> ret_stmt @@ addi (fnc_accessor _0 fd a, (const c_int32 1)))
let func1_str = show_func_with_dep func1

let func2 = make_fn ~cname:"myfunc1" c_int32 c_int32 (func0 @& func1 @& empty_fn)
  (fun fd a -> ret_stmt @@ addi (fnc_accessor _0 fd a, fnc_accessor _1 fd a))

let func2_str = show_func_with_dep func2


(* Recursive Function *)
let func_rec ci = 
  make_fn_rec 
  ~cname:"fibonacci" 
  ci
  ci
  empty_fn 
  (fun self _ arg -> 
    if_stmt 
      (greqi (arg, (const ci 0))) 
      (if_stmt (lei (arg, (const ci 2))) (ret_stmt @@ const ci 1) 
        (ret_stmt @@ addi (self (subi (arg, (const ci 1))), self (subi (arg, (const ci 2)))))) 
      (ret_stmt @@ const ci (-1))
  )
let func_rec_i32 = func_rec c_int32
let func_rec_i64 = func_rec c_int64
let func_rec_i32_str = show_func_with_dep func_rec_i32
let func_rec_i64_str = show_func_with_dep func_rec_i64
let my_simple_main = make_fn c_void c_int64 
(func_rec_i32 @& func_rec_i64 @& func1 @& func2 @& empty_fn)
(fun _ _ -> seq_stmt [
   ret_stmt @@ const c_int64 0
   ])
let main = make_prog_simple my_simple_main