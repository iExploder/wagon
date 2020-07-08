module type Float256 = sig
  type 't
  val make : float * float * float * float -> 't
  val add  : 't -> 't -> 't
  val mul  : 't -> 't -> 't
  val sum  : 't -> float
   .,..
end

module type Float128 = sig
  type 't
  val make : float * float -> 't
  val add  : 't -> 't -> 't
  val mul  : 't -> 't -> 't
  val sum  : 't -> float
   .,..
end

module type C_Dsl (F128 : Float128) (F256: Float256) ... = sig
  type stmt
  type 'a expr
  type ('arg,'ret) fnct

  type float128 = F128.t
  type float256 = F256.t

  type 'a c_typ =
  | Int : int c_typ
  | Float : float c_typ
  | Float128 : float128 c_typ
  | Float256 : float256 c_typ 

  val expr_stmt : 'a expr -> stmt
  val if_smt    : bool expr -> stmt -> stmt -> stmt
  val seq_smt   : stmt list -> stmt
  val ret_smt   : 'a expr -> stmt

  val cst       : 'a c_typ -> 'a -> 'a expr

  val mk_fun    : c_typ ('arg expr -> 'ret expr -> stmt) -> ('arg,'ret) fnct
end                                 

(*
mk_fun ([Int c_typ; Float c_typ],Int c_typ) ->  (fun [x;y] ->  ret_stmt (add x y)) ==>
  "int foo (int x, int y) {                                                               
   return (x+y);
   }"

module F128_C : Float128 = struct
  ?   intrinsics をつかった Cのプログラムを文字列として生成する
end
 *)

module C : C_Dsl = struct
   type stmt = string 
   type 'a expr = string
end


_____________________________________________-


let f x = afdadfa     f : 'a C_Dsl.expr -> 'a C_Dsl.expr
let g x = afdadfa     g : 'a C_Dsl.expr -> 'b C_DSL.expr -> ('a*'b) C_Dsl.expr


g (f xx) (f yy)  :  (Int,Float) C_Dsl.fnct

==>

C module で解釈したら


"
float anonymous (int x) {
  ...
}
"
