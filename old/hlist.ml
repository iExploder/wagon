
module type hlist = sig
  type 'a el                            (* element type *)
  type _ hlist =
    | Z : unit hlist
    | S : 'a el * 'b hlist -> ('a * 'b) hlist

  val nil  : unit hlist
  val cons : 'a el -> 'b hlist -> ('a * 'b) hlist
end

module HList(E: sig type 'a t end) = struct
  type 'a el = 'a E.t
  type _ hlist =
    | Z : unit hlist
    | S : 'a el * 'b hlist -> ('a * 'b) hlist

  let nil = Z
  let cons h t = S (h,t)
end

(* Simple HList, just for elements of type 'a *)
module HL = HList(struct type 'a t = 'a end)