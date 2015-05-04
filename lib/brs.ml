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
    let random_step _ b rules =
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
		       	     
(* Priorities *)	     
module PT = struct
    type t = RT.t list
    let f_val _ = true
    let f_r_val _ = true
  end

type graph = { v : (Big.bg_key, (int * Big.bg)) Hashtbl.t;
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
    type edge_type = R.edge	       
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
    type t = int
    type occ = R.occ
    let init = 0
    let increment t _ = t + 1
    let is_greater = ( > )
  end
	     
include TsType.Make (R)	(PriType.Make (R) (PT)) (L) (G) (S)
