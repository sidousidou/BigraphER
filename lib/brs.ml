(* Reaction rules *)
type react =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option         (* Instantiation map *)
  }

module RT = struct
    type t = react	
    type label = Epsilon  (* Empty label *)  
    type occ = Big.bg
    type edge = int
		  
    let lhs r = r.rdx
    let rhs r = r.rct
    let l _ = Epsilon
    let map r = r.eta
    let string_of_label _ = ""
    let val_chk _ = true
    let to_occ b _ = b
    let big_of_occ b = b
    let merge_occ b _ = b
    let update_occ _ b = b
    let edge_of_occ _ i = i
    let random_step b rules =
      (* Remove element with index i *)
      let rec aux i i' acc = function
	| [] -> assert false
	| x :: l -> if i = i' then (x, l @ acc)
		    else aux i (i' + 1) (x :: acc) l in
      let t = Sparse.trans b.Big.p.Place.nn in
      let rec _random_step b m = function
	| [] -> (None, m)
	| rs ->
	   (let (r, rs') =
	      aux (Random.int (List.length rs)) 0 [] rs in
	    match Big.occurrence b (lhs r) t with
	    | Some o ->
	       (Some (Big.rewrite o b (lhs r) (rhs r) (map r)), m + 1)
	    | None -> _random_step b (m + 1) rs') in
      _random_step b 0 rules
  end

module R = RrType.Make (RT)
		       
include R
	  
(* Override some functions *)	       
let to_string_react = R.to_string
			
let is_valid_react = R.is_valid
			  
let fix = R.fix

let step = R.step

let random_step = R.random_step
	     
(* Priorities *)	     
module PT = struct
    type t = RT.t list
    let f_val _ = true
    let f_r_val _ = true
  end

module P = PriType.Make (R) (PT)

include P			
			
(* Override some functions *)     
let is_valid_priority = is_valid
			  
let is_valid_priority_list = is_valid_list

let rewrite = rewrite

type stats = TsType.stats

type ts_g =
  { v : (Big.bg_key, (int * Big.bg)) Hashtbl.t;
    e : (int, int) Hashtbl.t;
    l : (int, int) Hashtbl.t;
  }
		  
module G = struct
    type t = ts_g
    type edge_type = RT.edge	       
    let init n =
      { v = Hashtbl.create n;
	e = Hashtbl.create n;
	l = Hashtbl.create n; }		      
    let states g = g.v
    let label g = g.l
    let edges g = g.e
    let dest u = u
    let string_of_arrow _ = ""
  end
	       
(* Transition system *)    
module TransitionSystem = TsType.MakeTS (R) (P) (G)
		      
(* Simulation trace *)
module L = struct
    type t = int
    type occ = Big.bg
    let init = 0
    let increment t _ = t + 1
    let is_greater = ( > )
  end
					
module Trace = TsType.MakeTrace (R) (P) (L) (G)
