open WagonC
module WagonAVX = struct
  open WagonC
  open Printf


  type mask8 = {mask8 : int}
  let _show_m8 x = sprintf "%#x" x.mask8
  let make_m8 : int -> mask8 = fun x -> if x < 0 || x > 0xff then failwith "unexpected mask8" else {mask8 = x}

  type 'a intrinb1 = 'a expr * 'a expr -> 'a expr
  type 'a intrinbm1 = 'a expr * 'a expr * mask8 -> 'a expr
  type 'a intrint1 = 'a expr * 'a expr * 'a expr -> 'a expr
  type ('a, 'b) intrinr1 = 'a 
  let _intrin = Obj.magic
  let _ib1 i (a,b) = sprintf "%s(%s, %s)" i a b
  let _it1 i (a,b,c) = sprintf "%s(%s, %s, %s)" i a b c
  let _ibm1 i (a,b,m) = sprintf "%s(%s, %s, %s)" i a b (_show_m8 m)


  (* Addition *)
  let addv256i8  : c_int8 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_add_epi8"
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
  let subv256i8  : c_int8 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_sub_epi8"
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
  let fmaddv256f32 : c_float32 c_v256 intrint1 = _intrin @@ _it1 "_mm256_fmadd_ps"
  let fmaddv256f64 : c_float64 c_v256 intrint1 = _intrin @@ _it1 "_mm256_fmadd_pd"
  let fmsubv256f32 : c_float32 c_v256 intrint1 = _intrin @@ _it1 "_mm256_fmsub_ps"
  let fmsubv256f64 : c_float64 c_v256 intrint1 = _intrin @@ _it1 "_mm256_fmsub_pd"
  let fmaddsubv256f32 : c_float32 c_v256 intrint1 = _intrin @@ _it1 "_mm256_fmaddsub_ps"
  let fmaddsubv256f64 : c_float64 c_v256 intrint1 = _intrin @@ _it1 "_mm256_fmaddsub_pd"
  let fmsubaddv256f32 : c_float32 c_v256 intrint1 = _intrin @@ _it1 "_mm256_fmsubadd_ps"
  let fmsubaddv256f64 : c_float64 c_v256 intrint1 = _intrin @@ _it1 "_mm256_fmsubadd_pd"
  let fmaddv512f32 : c_float32 c_v512 intrint1 = _intrin @@ _it1 "_mm512_fmadd_ps"
  let fmaddv512f64 : c_float64 c_v512 intrint1 = _intrin @@ _it1 "_mm512_fmadd_pd"
  let fmsubv512f32 : c_float32 c_v512 intrint1 = _intrin @@ _it1 "_mm512_fmsub_ps"
  let fmsubv512f64 : c_float64 c_v512 intrint1 = _intrin @@ _it1 "_mm512_fmsub_pd"
  let fmaddsubv512f32 : c_float32 c_v512 intrint1 = _intrin @@ _it1 "_mm512_fmaddsub_ps"
  let fmaddsubv512f64 : c_float64 c_v512 intrint1 = _intrin @@ _it1 "_mm512_fmaddsub_pd"
  let fmsubaddv512f32 : c_float32 c_v512 intrint1 = _intrin @@ _it1 "_mm512_fmsubadd_ps"
  let fmsubaddv512f64 : c_float64 c_v512 intrint1 = _intrin @@ _it1 "_mm512_fmsubadd_pd"

  (* Horizontal Addition/Subtraction *)
  let haddv256i16 : c_int16 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_hadd_epi16"
  let haddv256i32 : c_int32 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_hadd_epi32"
  let haddv256f32 : c_float32 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_hadd_ps"
  let haddv256f64 : c_float64 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_hadd_pd"
  let hsubv256i16 : c_int16 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_hsub_epi16"
  let hsubv256i32 : c_int32 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_hsub_epi32"
  let hsubv256f32 : c_float32 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_hsub_ps"
  let hsubv256f64 : c_float64 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_hsub_pd"


  let reduceaddv512i32 : c_int32 c_ityp c_v512 expr -> c_int32 c_ityp c_vdt expr = _intrin @@ Printf.sprintf "_mm512_reduce_add_epi32(%s)"
  let reduceaddv512i64 : c_int64 c_ityp c_v512 expr -> c_int64 c_ityp c_vdt expr = _intrin @@ Printf.sprintf "_mm512_reduce_add_epi64(%s)"
  let reduceaddv512f32 : c_float32 c_v512 expr -> c_float32 c_vdt expr = _intrin @@ Printf.sprintf "_mm512_reduce_add_ps(%s)"
  let reduceaddv512f64 : c_float64 c_v512 expr -> c_float64 c_vdt expr = _intrin @@ Printf.sprintf "_mm512_reduce_add_pd(%s)"
  

  (* Unpack *)
  
  let unpacklov256i8 : c_int8 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpacklo_epi8"
  let unpacklov256i16 : c_int16 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpacklo_epi16"
  let unpacklov256i32 : c_int32 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpacklo_epi32"
  let unpacklov256i64 : c_int64 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpacklo_epi64"
  let unpacklov256f32 : c_float32 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpacklo_ps"
  let unpacklov256f64 : c_float64 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpacklo_pd"
  let unpackhiv256i8 : c_int8 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpackhi_epi8"
  let unpackhiv256i16 : c_int16 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpackhi_epi16"
  let unpackhiv256i32 : c_int32 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpackhi_epi32"
  let unpackhiv256i64 : c_int64 c_ityp c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpackhi_epi64"
  let unpackhiv256f32 : c_float32 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpackhi_ps"
  let unpackhiv256f64 : c_float64 c_v256 intrinb1 = _intrin @@ _ib1 "_mm256_unpackhi_pd"

  let unpacklov512i8 : c_int8 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpacklo_epi8"
  let unpacklov512i16 : c_int16 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpacklo_epi16"
  let unpacklov512i32 : c_int32 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpacklo_epi32"
  let unpacklov512i64 : c_int64 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpacklo_epi64"
  let unpacklov512f32 : c_float32 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpacklo_ps"
  let unpacklov512f32x2 : c_float32 c_v512 intrinb1 = _intrin @@ 
    (fun (x,y) -> sprintf "_mm512_castpd_ps(_mm512_unpacklo_pd(_mm512_castps_pd(%s),_mm512_castps_pd(%s)))" x y)
  let unpacklov512f64 : c_float64 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpacklo_pd"
  let unpackhiv512i8 : c_int8 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpackhi_epi8"
  let unpackhiv512i16 : c_int16 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpackhi_epi16"
  let unpackhiv512i32 : c_int32 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpackhi_epi32"
  let unpackhiv512i64 : c_int64 c_ityp c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpackhi_epi64"
  let unpackhiv512f32 : c_float32 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpackhi_ps"
  let unpackhiv512f64 : c_float64 c_v512 intrinb1 = _intrin @@ _ib1 "_mm512_unpackhi_pd"
  let unpackhiv512f32x2 : c_float32 c_v512 intrinb1 = _intrin @@ 
    (fun (x,y) -> sprintf "_mm512_castpd_ps(_mm512_unpackhi_pd(_mm512_castps_pd(%s),_mm512_castps_pd(%s)))" x y)

  (* Shuffle *)
  let shufflev256i32x4 : c_int32 c_ityp c_v256 intrinbm1 = _intrin @@ _ibm1 "_mm256_shuffle_i32x4"
  let shufflev256i64x2 : c_int64 c_ityp c_v256 intrinbm1 = _intrin @@ _ibm1 "_mm256_shuffle_i64x2"
  let shufflev256f32 : c_float32 c_v256 intrinbm1 = _intrin @@ _ibm1 "_mm256_shuffle_ps"
  let shufflev256f64 : c_float64 c_v256 intrinbm1 = _intrin @@ _ibm1 "_mm256_shuffle_pd"
  let shufflev256f32x4 : c_float32 c_v256 intrinbm1 = _intrin @@ _ibm1 "_mm256_shuffle_f32x4"
  let shufflev256f64x2 : c_float64 c_v256 intrinbm1 = _intrin @@ _ibm1 "_mm256_shuffle_f64x2"
  let shufflev512i32x4 : c_int32 c_ityp c_v512 intrinbm1 = _intrin @@ _ibm1 "_mm512_shuffle_i32x4"
  let shufflev512i64x2 : c_int64 c_ityp c_v512 intrinbm1 = _intrin @@ _ibm1 "_mm512_shuffle_i64x2"
  let shufflev512f32 : c_float32 c_v512 intrinbm1 = _intrin @@ _ibm1 "_mm512_shuffle_ps"
  let shufflev512f64 : c_float64 c_v512 intrinbm1 = _intrin @@ _ibm1 "_mm512_shuffle_pd"
  let shufflev512f32x4 : c_float32 c_v512 intrinbm1 = _intrin @@ _ibm1 "_mm512_shuffle_f32x4"
  let shufflev512f64x2 : c_float64 c_v512 intrinbm1 = _intrin @@ _ibm1 "_mm512_shuffle_f64x2"
end