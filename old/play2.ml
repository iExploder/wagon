let _cons_none : ('b, 'c) wfus' -> ('a, 'b) wfus' -> ('a, 'c) wfus' =
  fun l r k -> l (fun sa -> r (fun sb -> k (sa ^ sb)))
let _lbk  : ('a, 'a) wfus' = fun k -> k "{"
let _rbk  : ('a, 'a) wfus' = fun k -> k "}"