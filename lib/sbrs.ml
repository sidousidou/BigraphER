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
    let update_occ (_, rho) b' = (b', rho)
    let edge_of_occ (_, rho) i = (i, rho)
    let random_step step_f b rules =
      (* Sort transitions by rate *)
      let (ss, m) = step_f b rules in
      let ss_sorted =
	List.fast_sort (fun a b ->
    			compare (snd a) (snd b))
		       ss in
      (* Compute exit rate *)
      let a0 =
    	List.fold_left (fun acc (_, rho) -> acc +. rho) 0.0 ss in
      let r = (Random.float 1.0) *. a0 in
      let rec aux acc = function
    	| (s, rho) :: ss ->
    	   let acc' = acc +. rho in
    	   if acc' > r then
             (let tau =
		(1. /. a0) *. (log (1. /. (Random.float 1.0))) in
              (Some (s, tau), m))
	   else aux acc' ss
    	| [] -> (None, m) in
      aux 0.0 ss_sorted
  end

module R = RrType.Make (RT)
		       
let is_inst r = R.l r = infinity
	     
module PT = struct
    type t = R.t list
    let f_val rr = not (List.exists is_inst rr)
    let f_r_val = List.for_all is_inst				   
  end

type graph = { v : (Big.bg_key, (int * Big.bg)) Hashtbl.t;
	       (* p : (int, Bilog) Hashtbl.t Predicates *)
	       e : (int, R.edge) Hashtbl.t;
	       l : (int, int) Hashtbl.t;  
	     }

type stats =  { time : float; 
		states : int;  
		trans : int;  
		occs : int;
	      }

module G = struct
    type t = graph
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

module S = struct
    type t = stats
    type g = graph
    let make t0 g m =
      { time = t0 -. (Unix.gettimeofday ());
	states = Hashtbl.length g.v; 
	trans = Hashtbl.length g.e;
	occs = m; }
end	       
	     
module L = struct
    type t = float
    type occ = R.occ
    let init = 0.0
    let increment t o = t +. (snd (R.edge_of_occ o 0))
    let is_greater = ( > )
  end
					
include TsType.Make (R) (PriType.Make (R) (PT)) (L) (G) (S)
