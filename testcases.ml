#require "str"
#use "WagonC.ml"
#use "WagonAVX.ml"
open WagonC
open WagonC

let mystring = c_string 10
let mystring_expr = const mystring "123\t56789"
let mystring_expr_str = show_expr mystring_expr

(* Structures *)
let mystruct = make_struct (c_int32 @^ c_int64 @^ c_float32 @^ empty_mtyp)
let mystruct_expr = const mystruct 1 2 3.0
let myargs = make_args (c_int32 @^ c_int64 @^ c_float32 @^ empty_mtyp)

let myargs_expr = const myargs 1 2 3.0

let _ = show_expr mystruct_expr;;
let _ = show_expr myargs_expr;;

#use "MTypeAccessor.ml"

open MTypeAccessor
open MTypeAccessor

let mystruct_expr_sub = struct_accessor _2 mystruct_expr
let _ = show_expr mystruct_expr_sub



(* Example Function *)

(* let func0 = fun x -> x + 1 *)
let func0 = make_fn c_int32 c_int32 empty_fn (fun _ a -> ret_stmt @@ addi (a, (const c_int32 1)))
let func0_str = show_func_with_dep func0

(* let func1 = fun x -> (func0 x) + 1 *)
let func1 = make_fn ~cname:"myfunc1" c_int32 c_int32 (func0 @& empty_fn) 
  (fun fd a -> ret_stmt @@ addi (fnc_accessor _0 fd a, (const c_int32 1)))
let func1_str = show_func_with_dep func1

(* let func2 = fun x -> (func0 x) + (func1 x) *)
let func2 = make_fn ~cname:"myfunc1" c_int32 c_int32 (func1 @& empty_fn)
  (fun fd a -> ret_stmt @@ addi (const c_int32 1, fnc_accessor _0 fd a))

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


(* my_simple_main: int64->int64 *)
let my_simple_main = make_fn c_int64 c_int64 empty_fn 
(fun _ _ -> ret_stmt @@ const c_int64 0)
(* my_simple_main2: void->int64 *)
let my_simple_main2 = make_fn c_void c_int64 (my_simple_main @& empty_fn)
(fun fhl _ ->
  let f = fnc_accessor _0 fhl in 
  ret_stmt @@ f (const c_int64 0))

let my_prog = make_prog_simple my_simple_main2

let _ = output_prog my_prog |> print_string

let matrixvf32 = (* (f32x16)x16 *)
  make_struct @@ 
  make_vec512c c_float32 @^ make_vec512c c_float32 @^ make_vec512c c_float32 @^ make_vec512c c_float32 @^ 
  make_vec512c c_float32 @^ make_vec512c c_float32 @^ make_vec512c c_float32 @^ make_vec512c c_float32 @^ 
  make_vec512c c_float32 @^ make_vec512c c_float32 @^ make_vec512c c_float32 @^ make_vec512c c_float32 @^ 
  make_vec512c c_float32 @^ make_vec512c c_float32 @^ make_vec512c c_float32 @^ make_vec512c c_float32 @^ 
  empty_mtyp

let matrixvf32_arr =
  make_array (make_vec512c c_float32) 16



open WagonAVX
open WagonAVX

let matrixvf32_arr_cref = make_cref matrixvf32_arr
let fx_transposev512f32 =
  make_fn ~cname:"transpose_v512f32" matrixvf32_arr matrixvf32_arr empty_fn
  (fun _ args ->
    decl_stmt matrixvf32_arr 
      (Array.init 16
        (fun i -> 
          if i mod 2 = 0 then 
            unpacklov512f32 (sub_arr matrixvf32_arr args (const c_int64 (i / 2 * 2)),
                             sub_arr matrixvf32_arr args (const c_int64 (i / 2 * 2 + 1)))
          else 
            unpackhiv512f32 (sub_arr matrixvf32_arr args (const c_int64 (i / 2 * 2)),
                             sub_arr matrixvf32_arr args (const c_int64 (i / 2 * 2 + 1)))) |> const matrixvf32_arr)
      (fun t -> 
        decl_stmt matrixvf32_arr (
          Array.init 16
          (fun i ->
            if i mod 2 = 0 then
              unpacklov512f32x2 (sub_arr matrixvf32_arr (delval t) (const c_int64 (i/4*4+(i mod 4)/2)),
              sub_arr matrixvf32_arr (delval t) (const c_int64 (i/4*4+(i mod 4)/2+2)))
            else 
              unpackhiv512f32x2 (sub_arr matrixvf32_arr (delval t) (const c_int64 (i/4*4+(i mod 4)/2)),
              sub_arr matrixvf32_arr (delval t) (const c_int64 (i/4*4+(i mod 4)/2+2)))) |> const matrixvf32_arr)
          
          (fun r ->
            seq_stmt @@
            (List.init 4 
            (fun i -> assign_stmt (sub_arrl matrixvf32_arr t @@ const c_int64 i) 
              (shufflev512f32x4 (sub_arr matrixvf32_arr (delval r) @@ const c_int64 i, sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+4), make_m8 0x88))))
            @
            (List.init 4 
            (fun i -> assign_stmt (sub_arrl matrixvf32_arr t @@ const c_int64 (i+4)) 
              (shufflev512f32x4 (sub_arr matrixvf32_arr (delval r) @@ const c_int64 i, sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+4), make_m8 0xdd))))
            @
            (List.init 4
            (fun i -> assign_stmt (sub_arrl matrixvf32_arr t @@ const c_int64 (i+8)) 
              (shufflev512f32x4 (sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+8), sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+12), make_m8 0x88))))
            @
            (List.init 4 
            (fun i -> assign_stmt (sub_arrl matrixvf32_arr t @@ const c_int64 (i+12)) 
              (shufflev512f32x4 (sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+8), sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+12), make_m8 0xdd))))
            @

            (List.init 8 
            (fun i -> assign_stmt (sub_arrl matrixvf32_arr r @@ const c_int64 i) 
              (shufflev512f32x4 (sub_arr matrixvf32_arr (delval t) @@ const c_int64 i, sub_arr matrixvf32_arr (delval t) @@ const c_int64 (i+8), make_m8 0x88))))
            @
            (List.init 8 
            (fun i -> assign_stmt (sub_arrl matrixvf32_arr r @@ const c_int64 (i+8)) 
              (shufflev512f32x4 (sub_arr matrixvf32_arr (delval t) @@ const c_int64 i, sub_arr matrixvf32_arr (delval t) @@ const c_int64 (i+8), make_m8 0xdd))))
            @
            [ret_stmt @@ delval r]

          )
        )

  )


  let fx_transposemv512f32 =
    make_fn ~cname:"transpose_mv512f32" 
    (make_args @@ matrixvf32_arr_cref @^ matrixvf32_arr @^ empty_mtyp) c_int64 empty_fn
    (fun _ args ->
      let dst = marg_accessor _0 args in
      let src = marg_accessor _1 args in
      decl_stmt matrixvf32_arr 
        (Array.init 16
          (fun i -> 
            if i mod 2 = 0 then 
              unpacklov512f32 (sub_arr matrixvf32_arr src (const c_int64 (i / 2 * 2)),
                               sub_arr matrixvf32_arr src (const c_int64 (i / 2 * 2 + 1)))
            else 
              unpackhiv512f32 (sub_arr matrixvf32_arr src (const c_int64 (i / 2 * 2)),
                               sub_arr matrixvf32_arr src (const c_int64 (i / 2 * 2 + 1)))) |> const matrixvf32_arr)
        (fun t -> 
            let r = deref dst in
            seq_stmt (
              (
              List.init 16
                (fun i ->
                assign_stmt (sub_arrl matrixvf32_arr r (const c_int64 i))
                (if i mod 2 = 0 then
                  unpacklov512f32x2 (sub_arr matrixvf32_arr (delval t) (const c_int64 (i/4*4+(i mod 4)/2)),
                  sub_arr matrixvf32_arr (delval t) (const c_int64 (i/4*4+(i mod 4)/2+2)))
                else 
                  unpackhiv512f32x2 (sub_arr matrixvf32_arr (delval t) (const c_int64 (i/4*4+(i mod 4)/2)),
                  sub_arr matrixvf32_arr (delval t) (const c_int64 (i/4*4+(i mod 4)/2+2))))
                  ) 
                
              )
              @
              (List.init 4 
              (fun i -> assign_stmt (sub_arrl matrixvf32_arr t @@ const c_int64 i) 
                (shufflev512f32x4 (sub_arr matrixvf32_arr (delval r) @@ const c_int64 i, sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+4), make_m8 0x88))))
              @
              (List.init 4 
              (fun i -> assign_stmt (sub_arrl matrixvf32_arr t @@ const c_int64 (i+4)) 
                (shufflev512f32x4 (sub_arr matrixvf32_arr (delval r) @@ const c_int64 i, sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+4), make_m8 0xdd))))
              @
              (List.init 4
              (fun i -> assign_stmt (sub_arrl matrixvf32_arr t @@ const c_int64 (i+8)) 
                (shufflev512f32x4 (sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+8), sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+12), make_m8 0x88))))
              @
              (List.init 4 
              (fun i -> assign_stmt (sub_arrl matrixvf32_arr t @@ const c_int64 (i+12)) 
                (shufflev512f32x4 (sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+8), sub_arr matrixvf32_arr (delval r) @@ const c_int64 (i+12), make_m8 0xdd))))
              @
  
              (List.init 8 
              (fun i -> assign_stmt (sub_arrl matrixvf32_arr r @@ const c_int64 i) 
                (shufflev512f32x4 (sub_arr matrixvf32_arr (delval t) @@ const c_int64 i, sub_arr matrixvf32_arr (delval t) @@ const c_int64 (i+8), make_m8 0x88))))
              @
              (List.init 8 
              (fun i -> assign_stmt (sub_arrl matrixvf32_arr r @@ const c_int64 (i+8)) 
                (shufflev512f32x4 (sub_arr matrixvf32_arr (delval t) @@ const c_int64 i, sub_arr matrixvf32_arr (delval t) @@ const c_int64 (i+8), make_m8 0xdd))))
              @
              [ret_stmt @@ const c_int64 0]
            )
        )
    )


let func_multiplym512v32 =
  make_fn ~cname:"multiplymv512f32" 
  (make_args @@ 
    (make_array (make_vec512 c_float32) 16 |> make_cref) 
    @^ (make_array (make_vec512 c_float32) 16 |> make_cref) 
    @^ (make_array (make_vec512 c_float32) 16 |> make_cref) 
    @^ empty_mtyp)
    c_int64
    empty_fn
    (fun _ args ->
      let pdst = marg_accessor _0 args in 
      let plhs = marg_accessor _1 args in 
      let prhs = marg_accessor _2 args in
      seq_stmt @@ 
      List.init (16*16) 
      (fun i ->
        let arrtype = (make_array (make_vec512 c_float32) 16) in
        let vectype = (make_vec512 c_float32) in
        let x = i mod 16 and y = i / 16 in
        assign_stmt (sub_vec512l vectype (sub_arrl arrtype (deref pdst) (const c_int64 y)) (const c_int64 x)) 
          (reduceaddv512f32 (mulv512f32 
            ((sub_arr arrtype (delval @@ deref plhs) (const c_int64 x)),
            (sub_arr arrtype (delval @@ deref prhs) (const c_int64 y)))) ) )
      @ [ret_stmt @@ const c_int64 0])

  



let func_transposev512f32 = 

  make_fn ~cname:"transposev512f32" matrixvf32 matrixvf32 empty_fn
(fun _ args -> 
  decl_stmt matrixvf32 (const matrixvf32 
    (unpacklov512f32 (struct_accessor _0 args , struct_accessor _1 args ))
    (unpackhiv512f32 (struct_accessor _0 args , struct_accessor _1 args ))
    (unpacklov512f32 (struct_accessor _2 args , struct_accessor _3 args ))
    (unpackhiv512f32 (struct_accessor _2 args , struct_accessor _3 args ))
    (unpacklov512f32 (struct_accessor _4 args , struct_accessor _5 args ))
    (unpackhiv512f32 (struct_accessor _4 args , struct_accessor _5 args ))
    (unpacklov512f32 (struct_accessor _6 args , struct_accessor _7 args ))
    (unpackhiv512f32 (struct_accessor _6 args , struct_accessor _7 args ))
    (unpacklov512f32 (struct_accessor _8 args , struct_accessor _9 args ))
    (unpackhiv512f32 (struct_accessor _8 args , struct_accessor _9 args ))
    (unpacklov512f32 (struct_accessor _10 args , struct_accessor _11 args ))
    (unpackhiv512f32 (struct_accessor _10 args , struct_accessor _11 args ))
    (unpacklov512f32 (struct_accessor _12 args , struct_accessor _13 args ))
    (unpackhiv512f32 (struct_accessor _12 args , struct_accessor _13 args ))
    (unpacklov512f32 (struct_accessor _14 args , struct_accessor _15 args ))
    (unpackhiv512f32 (struct_accessor _14 args , struct_accessor _15 args ))
    
  )(fun t ->
    seq_stmt [
      ret_stmt @@ delval t
    ]
  )
  )

  let func_v512f64_dotp = make_fn ~cname:"v512f64_dotp" (make_args @@ (make_array (make_vec512 c_float64) 100) @^ (make_array (make_vec512 c_float64) 100) @^ c_int64 @^ empty_mtyp) c_float64 empty_fn
  (fun _ args ->
    let v512f64arr = make_array (make_vec512 c_float64) 100 in
    let va = marg_accessor _0 args in
    let vb = marg_accessor _1 args in
    let len = marg_accessor _2 args in
    let v512f64 = make_vec512 c_float64 in
    decl_stmt v512f64 (const v512f64 [|const c_float64 0.0;const c_float64 0.0;const c_float64 0.0;const c_float64 0.0;const c_float64 0.0;const c_float64 0.0;const c_float64 0.0;const c_float64 0.0;|])
    (fun t ->

        decl_stmt c_int64 (const c_int64 0)
        (fun i ->
          seq_stmt [
            for_stmt (lei (delval i, len), assign_expr i (addi (delval i, const c_int64 1)), 
              assign_stmt t @@ fmaddv512f64 (sub_arr v512f64arr va (delval i), sub_arr v512f64arr vb (delval i), delval t));
            ret_stmt @@ reduceaddv512f64 (delval t)
          ]
        )
      )
  )

let func_gaussian_blur_box = 0