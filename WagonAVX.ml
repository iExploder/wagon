open WagonC
module WagonAVX = struct
  open WagonC
  open Printf
  type 'a intrinb1 = 'a expr * 'a expr -> 'a expr
  type 'a intrint1 = 'a expr * 'a expr * 'a expr -> 'a expr
  let _intrin = Obj.magic
  let _ib1 i (a,b) = sprintf "%s(%s, %s)" i a b
  let _it1 i (a,b,c) = sprintf "%s(%s, %s, %s)" i a b c


  (* Addition *)
  let addv256i8  : (c_int8 c_ityp c_v256) intrinb1 = _intrin @@ _ib1 "_mm256_add_epi8"
  let addv256i16 : c_int16 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_add_epi16"
  let addv256i32 : c_int32 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_add_epi32"
  let addv256i64 : c_int64 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_add_epi64"
  let addv256f32 : c_float32 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_add_ps"
  let addv256f64 : c_float64 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_add_pd"
  let addv512i8  : (c_int8 c_ityp c_v512) intrinb1 = _intrin @@ _ib1 "_mm512_add_epi8"
  let addv512i16 : c_int16 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_add_epi16"
  let addv512i32 : c_int32 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_add_epi32"
  let addv512i64 : c_int64 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_add_epi64"
  let addv512f32 : c_float32 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_add_ps"
  let addv512f64 : c_float64 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_add_pd"

  (* Subtraction *)
  let subv256i8  : (c_int8 c_ityp c_v256) intrinb1 = _intrin @@ _ib1 "_mm256_sub_epi8"
  let subv256i16 : c_int16 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_sub_epi16"
  let subv256i32 : c_int32 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_sub_epi32"
  let subv256i64 : c_int64 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_sub_epi64"
  let subv256f32 : c_float32 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_sub_ps"
  let subv256f64 : c_float64 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_sub_pd"
  let subv512i8  : (c_int8 c_ityp c_v512) intrinb1 = _intrin @@ _ib1 "_mm512_sub_epi8"
  let subv512i16 : c_int16 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_sub_epi16"
  let subv512i32 : c_int32 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_sub_epi32"
  let subv512i64 : c_int64 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_sub_epi64"
  let subv512f32 : c_float32 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_sub_ps"
  let subv512f64 : c_float64 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_sub_pd"

  (* Multiplication *)
  let mulv256i32 : c_int32 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_mul_epi32"
  let mulv256f32 : c_float32 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_mul_ps"
  let mulv256f64 : c_float64 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_mul_pd"
  let mulv512i32 : c_int32 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_mul_epi32"
  let mulv512f32 : c_float32 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_mul_ps"
  let mulv512f64 : c_float64 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_mul_pd"

  (* Division *)
  let divv256f32 : c_float32 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_div_ps"
  let divv256f64 : c_float64 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_div_pd"
  let divv512f32 : c_float32 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_div_ps"
  let divv512f64 : c_float64 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_div_pd"

  (* FMA = Fused Multiply-Add *)
  
end