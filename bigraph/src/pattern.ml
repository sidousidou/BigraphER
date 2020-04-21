(* A pattern represents a family of bigraphs *)

(* Parametrised sum types *)

(* Type of edge? *)

(* Set of controls *)

(* { A(2,x) with x > 4 & x < 7, B, C(a) with a = test* } *)

(*type ctrl = C of String.t * arg list * int*)

type p = {
  p : Place.t;
  (* Place graph *)
  l : Link.Lg.t;
  (* Link graph *)
  n : Nodes.t; (* Node set with *)
}

(* Elementary patterns *)

(* Operations *)

(* Matching against a bigraph *)
