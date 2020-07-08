module type WagonBasic =
  sig
    type wtname = string
    type 'a wterm = wtname
    type ('t, 'init) wtype = { typename : wtname; init : 'init -> wtname; }
    type wi8
    type wi16
    type wi32
    type wi64
    type wf32
    type wf64
    val wi8 : (wi8, int) wtype
    val wi16 : (wi16, int) wtype
    val wi32 : (wi32, int) wtype
    val wi64 : (wi64, int) wtype
    val wf32 : (wf32, float) wtype
    val wf64 : (wf64, float) wtype
    val warr : ('t, 'i) wtype -> int -> ('t array, 'i array) wtype
    type ('a, 'b) ws = (wtname -> 'a) -> 'b
    val typ : ('a, 'b) wtype -> ('c, 'b -> 'c) ws
    val ( <> ) : ('a, 'b) ws -> ('c, 'a) ws -> ('c, 'b) ws
    val wstruct : (wtname, wtname) ws -> wtname
  end