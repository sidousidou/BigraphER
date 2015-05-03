type sreact =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option;        (* Instantiation map *)
    rate : float
  }
       
module RT = struct
    type t = sreact
    type label = float 
    type occ = Big.bg * float
    type edge = int * float
			
    let lhs r = r.rdx
    let rhs r = r.rct
    let l r = r.rate
    let map r = r.eta
    let string_of_label l = string_of_float l
    let val_chk r = r.rate > 0.0
    let to_occ b r = (b, r.rate)
    let big_of_occ (b, _) = b
    let merge_occ (b, rho) (_, rho') = (b, rho +. rho')
    let update_occ (b, rho) b' = (b', rho)
    let edge_of_occ (b, rho) i = (i, rho)
    let random_step b rules = (None, 0)
    (* let random_step (s : Big.bg) srules m = *)
    (*   (\* sort transitions by rate *\) *)
    (*   let (ss, m') = step s srules in *)
    (*   let ss_sorted = List.fast_sort (fun a b -> *)
    (* 				      Pervasives.compare ((snd a) : float) ((snd b) : float)) ss in *)
    (*   (\* compute exit rate *\) *)
    (*   let a0 = *)
    (* 	List.fold_left (fun acc (_, rho) -> acc +. rho) 0.0 ss in *)
    (*   let r = (Random.float 1.0) *. a0 in *)
    (*   let rec aux (l : (Big.bg * float) list) (acc : float) = *)
    (* 	match l with *)
    (* 	| (s, rho) :: ss -> *)
    (* 	   let acc' = acc +. rho in  *)
    (* 	   if acc' > r then begin *)
    (*            let tau = (1. /. a0) *. (log (1. /. (Random.float 1.0))) in *)
    (*            (s, tau)  *)
    (* 	     end else aux ss acc' *)
    (* 	| [] -> raise (DEAD m) *)
    (*   in (aux ss_sorted 0.0, m + m') *)
  end

module R = RrType.Make (RT)

include R
		       
let to_string_react = R.to_string
			
let is_valid_react = R.is_valid
		       
let fix = R.fix
	    
let step = R.step

let random_step = R.random_step	     

let is_inst r = R.l r = infinity
	     
module PT = struct
    type t = R.t list
    let f_val rr = not (List.exists is_inst rr)
    let f_r_val = List.for_all is_inst				   
  end

module P = PriType.Make (R) (PT) 
	      
include P
	  
let is_valid_priority = is_valid
			  
let is_valid_priority_list = is_valid_list

let rewrite = rewrite			       

type stats = TsType.stats
		
type g = {
  v : (Big.bg_key, (int * Big.bg)) Hashtbl.t;
  (* p : (int, Bilog) Hashtbl.t Predicates *)
  e : (int, edge) Hashtbl.t;
  l : (int, int) Hashtbl.t;  
}

module G = struct
    type t = g
    type edge_type = RT.edge	       
    let init n =
      { v = Hashtbl.create n;
	e = Hashtbl.create n;
	l = Hashtbl.create n; }		      
    let states g = g.v
    let label g = g.l
    let edges g = g.e
    let dest u = fst u
    let string_of_arrow u = string_of_float (snd u)
  end

module Ctmc = TsType.MakeTS (R) (P) (G)

module L = struct
    type t = float
    type occ = R.occ
    let init = 0.0
    let increment t o = t +. (snd (R.edge_of_occ o 0))
    let is_greater = ( > )
  end
					
module Trace = TsType.MakeTrace (R) (P) (L) (G)
