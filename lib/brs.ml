(* Reaction rules *)
type react =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option         (* Instantiation map *)
  }

module RT = struct
    type t = react	
    type label = Epsilon       (* Empty label       *)  
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
  end

module R = RrType.Make (RT)
		       
include R
	  
(* Override some functions *)	       
let to_string_react = R.to_string
			
let is_valid_react = R.is_valid
			  
let fix = R.fix

let step = R.step

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
		
(* Transition system *)    
type ts_g =
  { v : (Big.bg_key, (int * Big.bg)) Hashtbl.t;
    e : (int, int) Hashtbl.t;
    l : (int, int) Hashtbl.t;
  }
		  
module TransitionSystem =
  TsType.MakeTS (R) (P)
		      (struct
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
			end)
		      
(* Simulation trace *)
(* module Trace = *)
(*   TsType.MakeTrace (R) (P) *)
(* 		      (struct *)
(* 			  type t = ts_g *)
(* 			  type edge_type = RT.edge *)
					     
(* 			  let init n = *)
(* 			    { v = Hashtbl.create n; *)
(* 			      e = Hashtbl.create n; *)
(* 			      l = Hashtbl.create n; }		       *)
(* 			  let states g = g.v *)
(* 			  let label g = g.l *)
(* 			  let edges g = g.e *)
(* 			  let dest u = u *)
(* 			  let string_of_arrow _ = "" *)
(* 			end) *)
(*   (struct	       *)

(*     (\* Remove element with index i *\) *)
(*     let rec aux i i' acc = function *)
(*       | [] -> (None, acc) *)
(*       | x :: l -> if i = i' then (Some x, l @ acc) *)
(* 		  else aux i (i' + 1) (x :: acc) l *)
			   
(*     (\* - Select a reaction rule randomly *)
(*        - Try to rewrite *)
(*        - Done on success, repeat with the remainig reaction rules otherwise *\)			   *)
(*     let random_step b rules = *)
(*       let t = Sparse.trans b.Big.p.Place.nn in *)
(*       let rec _random_step b = function *)
(* 	| [] -> None *)
(* 	| rs -> *)
(* 	   (let (r, rs') = *)
(* 	      aux (Random.int (List.length rs)) 0 [] rs in *)
(* 	    match r with *)
(* 	    | None -> assert false *)
(* 	    | Some r -> *)
(* 	       (match Big.occurrence b (R.lhs r) t with *)
(* 		| Some o -> *)
(* 		   Some (Big.rewrite o b (R.lhs r) (R.rhs r) (R.map r)) *)
(* 		| None -> _random_step b rs')) in *)
(*       _random_step b rules *)
(*   end) *)
		 
(* let init_ts n =  *)
(*   { v = Hashtbl.create n; *)
(*     e = Hashtbl.create n; *)
(*     l = Hashtbl.create n; *)
(*   } *)

(* let is_new b v = *)
(*   let k = Big.key b in *)
(*   let k_buket =  *)
(*     Hashtbl.find_all v k in *)
(*   try  *)
(*     let (old, _) = List.find (fun (_, b') -> *)
(*         (* printf "b =? b'\n\ *) *)
(*         (*         ----------------\n\ *) *)
(*         (*         %s\n\ *) *)
(*         (*         ----------------\n\ *) *)
(*         (*         %s\n\ *) *)
(*         (*         ----------------\n" (Big.to_string b) (Big.to_string b'); *) *)
(*         Big.equal b b') k_buket in *)
(*     raise (OLD old) *)
(*   with *)
(*   | Not_found -> true *)
 
(* (* Partition a list of bigraphs into new and old states *) *)
(* let _partition_aux ts i f_iter = *)
(*   List.fold_left (fun (new_acc, old_acc, i) b -> *)
(*     try  *)
(*       ignore (is_new b ts.v); *)
(*       let i' = i + 1 in *)
(*       f_iter i' b; *)
(*       ((i', b) :: new_acc, old_acc, i') *)
(*     with *)
(*       | OLD x -> (new_acc, x :: old_acc, i) *)
(*   ) ([], [], i) *)
    
(* (* Iterate over priority classes *) *)
(* let rec _scan step_f curr m ts i iter_f pl pl_const = *)
(*   match pl with *)
(*   | [] -> (([], [], i), m) *)
(*   | c :: cs -> *)
(*     begin *)
(*       match c with *)
(*       | P_class rr -> *)
(* 	begin *)
(* 	  let (ss, l) = step_f curr rr in *)
(* 	  if l = 0 then _scan step_f curr m ts i iter_f cs pl_const  *)
(* 	  else  *)
(* 	    (* apply rewriting *) *)
(* 	    let (ss', l') =  *)
(* 	      List.fold_left (fun (ss,  l) s ->  *)
(* 		  let (s', l') = rewrite s l pl_const in *)
(* 		  (s' :: ss, l') *)
(*                 ) ([], l) ss in *)
(* 	    (_partition_aux ts i iter_f ss', m + l') *)
(* 	end *)
(*       | P_rclass _ -> (* skip *) *)
(* 	_scan step_f curr m ts i iter_f cs pl_const *)
(*     end  *)

(* (* queue contains already a state *)  *)
(* let rec _bfs ts q i m (scan_f, step_f, p_classes, t0, limit, ts_size, iter_f) = *)
(*   if not (Queue.is_empty q) then  *)
(*     if i > limit  *)
(*     then (let stats =  *)
(* 	    { t = (Unix.gettimeofday ()) -. t0; *)
(* 	      s = Hashtbl.length ts.v;  *)
(* 	      r = Hashtbl.length ts.e; *)
(* 	      o = m *)
(* 	    } in *)
(* 	  raise (LIMIT (ts, stats))) *)
(*     else begin  *)
(*       let (v, curr) = Queue.pop q in *)
(*       let ((new_s, old_s, i'), m') =  *)
(* 	scan_f step_f curr m ts i iter_f p_classes p_classes in *)
(*       (* Add new states to v *) *)
(*       List.iter (fun (i, b) ->  *)
(* 	Hashtbl.add ts.v (Big.key b) (i, b)) new_s; *)
(*       (* Add new states to q *) *)
(*       List.iter (fun s -> Queue.push s q) new_s; *)
(*       (* Add labels for new states *) *)
(*       (* TO DO *) *)
(*       (* Add edges from v to new states *) *)
(*       List.iter (fun (u, _) -> Hashtbl.add ts.e v u) new_s; *)
(*       (* Add edges from v to old states *) *)
(*       List.iter (fun u -> Hashtbl.add ts.e v u) old_s; *)
(*       (* recursive call *) *)
(*       _bfs ts q i' m' (scan_f, step_f, p_classes, t0, limit, ts_size, iter_f) *)
(*     end *)
(*   else let stats =  *)
(* 	 { t = (Unix.gettimeofday ()) -. t0; *)
(* 	   s = Hashtbl.length ts.v;  *)
(* 	   r = Hashtbl.length ts.e; *)
(* 	   o = m; *)
(* 	 } in *)
(*        (ts, stats) *)

(* let _init_bfs s0 rewrite _scan step rules limit ts_size iter_f = *)
(*   let consts =  *)
(*     (_scan, step, rules, Unix.gettimeofday (), limit, ts_size, iter_f) *)
(*   and q = Queue.create () in *)
(*   (* apply rewriting to s0 *) *)
(*   let (s0', m) = rewrite s0 0 rules *)
(*   and ts = init_ts ts_size in *)
(*   Queue.push (0, s0') q; *)
(*   (* add initial state *) *)
(*   Hashtbl.add ts.v (Big.key s0') (0, s0'); *)
(*   (ts, s0', q, m, consts) *)
 
(* let bfs s0 rules limit ts_size iter_f = *)
(*   let (ts, s0', q, m, consts) =  *)
(*     _init_bfs s0 rewrite _scan step rules limit ts_size iter_f in *)
(*   iter_f 0 s0'; *)
(*   _bfs ts q 0 m consts     *)

(* let _sim_step b rules = *)
(*   match random_step b rules with *)
(*   | Some b -> ([b], 1) *)
(*   | None -> ([], 0) *)

(* let sim s0 rules limit ts_size iter_f = *)
(*   Random.self_init (); *)
(*   let (ts, s0', q, m, consts) = *)
(*     _init_bfs  *)
(*       s0 rewrite _scan _sim_step rules limit ts_size iter_f in *)
(*   iter_f 0 s0'; *)
(*   _bfs ts q 0 m consts  *)

(* let to_dot ts = *)
(*   let rank = "{ rank=source; 0 };\n" in *)
(*   let states = *)
(*     Hashtbl.fold (fun _ (i, _) buff ->  *)
(*       if i = 0 then Printf.sprintf  *)
(* 	"%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \ *)
(*                 fontname=\"monospace\", fixedsize=true, width=.60, height=.30 \ *)
(*                 style=\"bold\" ];\n"  *)
(* 	buff i i i i *)
(*       else Printf.sprintf  *)
(* 	"%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \ *)
(*                 fontname=\"monospace\", fixedsize=true, width=.60, height=.30 ];\n"  *)
(* 	buff i i i i *)
(*     ) ts.v "" *)
(*   and edges = *)
(*     Hashtbl.fold (fun v u buff ->  *)
(*       Printf.sprintf "%s%d -> %d [ arrowhead=\"vee\", arrowsize=0.5 ];\n" *)
(* 	buff v u) ts.e "" in *)
(*   Printf.sprintf "digraph ts {\nstylesheet = \"style_brs.css\"\n%s%s\n%s}" rank states edges *)

(* let to_prism ts = *)
(*   let dims = *)
(*     (Hashtbl.length ts.v, Hashtbl.length ts.e) *)
(*   and edges = *)
(*     Hashtbl.fold (fun v u acc -> *)
(* 		  (v, u) :: acc) ts.e [] in *)
(*   dims :: (List.fast_sort Base.ints_compare edges) *)
(*   |> List.map (fun (v, u) -> *)
(* 	       (string_of_int v) ^ " " ^ (string_of_int u)) *)
(*   |> String.concat "\n" *)

(* let to_lab ts = *)
(*   let inv = *)
(*     Hashtbl.create (Hashtbl.length ts.l) in *)
(*   Hashtbl.fold (fun s p acc ->  *)
(* 		Hashtbl.add inv p s; *)
(* 		p :: acc) *)
(* 	       ts.l [] *)
(*   |>  List.map (fun p -> *)
(* 		Hashtbl.find_all inv p *)
(* 		|> List.map (fun s -> "x = " ^ (string_of_int s))  *)
(* 		|> String.concat " | "  *)
(* 		|> fun s -> *)
(* 		   "label \"p_" ^ (string_of_int p) ^ "\" = " ^ s ^ ";") *)
(*   |> String.concat "\n" *)
		   
(* let iter_states f ts = *)
(*   Hashtbl.iter (fun _ (i, b) -> f i b) ts.v *)
