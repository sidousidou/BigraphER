type sreact =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option;        (* Instantiation map *)
    rate : float
  }
       
module R =
  RrType.Make (struct
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
		end)

let to_string_react = R.to_string
			
let is_valid_react = R.is_valid
		       
let fix = R.fix
	    
let step = R.step

let is_inst r = R.l r = infinity
	     
module P =
  PriType.Make (R) (struct
		       type t = R.t list
		       let f_val rr = not (List.exists is_inst rr)
		       let f_r_val = List.for_all is_inst				   
		     end)

include P
	  
let is_valid_priority = is_valid
			  
let is_valid_priority_list = is_valid_list

let rewrite = rewrite			       
	     
type ctmc = {
  v : (Big.bg_key, (int * Big.bg)) Hashtbl.t;
  (* p : (int, Bilog) Hashtbl.t Predicates *)
  e : (int, (int * float)) Hashtbl.t;
  l : (int, int) Hashtbl.t;  
}

type stats = {
  t : float;    (** Execution time *)
  sim : float;  (** Simulation time *)
  s : int;      (** Number of states *)
  r : int;      (** Number of reaction *)
  o : int;      (** Number of occurrences *)
}

(* raised when the size of the ts reaches the limit *)
exception LIMIT of ctmc * stats

(* raised when a deadlock is reached in a simulation *)
exception DEAD of int

(* let init_ctmc n =  *)
(*   { v = Hashtbl.create n; *)
(*     e = Hashtbl.create n; *)
(*     l = Hashtbl.create n; *)
(*   } *)

(* rule selection: second step of SSA 
   raise DEAD *)
let random_step (s : Big.bg) srules m =
  (* sort transitions by rate *)
  let (ss, m') = step s srules in
  let ss_sorted = List.fast_sort (fun a b ->
      Pervasives.compare ((snd a) : float) ((snd b) : float)) ss in
  (* compute exit rate *)
  let a0 =
    List.fold_left (fun acc (_, rho) -> acc +. rho) 0.0 ss in
  let r = (Random.float 1.0) *. a0 in
  let rec aux (l : (Big.bg * float) list) (acc : float) =
    match l with
    | (s, rho) :: ss ->
      let acc' = acc +. rho in 
      if acc' > r then begin
        let tau = (1. /. a0) *. (log (1. /. (Random.float 1.0))) in
        (s, tau) 
      end else aux ss acc'
    | [] -> raise (DEAD m)
  in (aux ss_sorted 0.0, m + m')

(* let is_new b v = *)
(*   let k = Big.key b in *)
(*   let k_buket =  *)
(*     Hashtbl.find_all v k in *)
(*   try  *)
(*     let (old, _) = List.find (fun (_, b') -> *)
(*         Big.equal b b') k_buket in *)
(*     raise (OLD old) *)
(*   with *)
(*   | Not_found -> true *)

(* (\* Partition a list of bigraphs into new and old states *\) *)
(* let _partition_aux ctmc i iter_f = *)
(*   List.fold_left (fun (new_acc, old_acc, i) (b, rho) -> *)
(*       try  *)
(*         ignore (is_new b ctmc.v); *)
(*         let i' = i + 1 in *)
(*         iter_f i' b; *)
(*         (((i', b), rho) :: new_acc, old_acc, i') *)
(*       with *)
(*       | OLD x -> (new_acc, (x, rho) :: old_acc, i) *)
(*     ) ([], [], i) *)

(* (\* Iterate over priority classes *\) *)
(* let rec _scan curr m ctmc i iter_f pl pl_const = *)
(*   match pl with *)
(*   | [] -> (([], [], i), m) *)
(*   | c :: cs -> ( *)
(*       match c with *)
(*       | P_class rr -> ( *)
(*           let (ss, l) = step curr rr in *)
(*           if l = 0 then _scan curr m ctmc i iter_f cs pl_const  *)
(*           else  *)
(*             (\* apply rewriting - instantaneous *\) *)
(*             let (ss', l') =  *)
(*               List.fold_left (fun (ss,  l) (s, rho) ->  *)
(*                   let (s', l') = rewrite s l pl_const in *)
(*                   ((s', rho) :: ss, l') *)
(*                 ) ([], l) ss in *)
(*             (_partition_aux ctmc i iter_f ss', m + l') *)
(*         ) *)
(*       | P_rclass _ -> (\* skip *\) *)
(*         _scan curr m ctmc i iter_f cs pl_const *)
(*     ) *)

(* let rec _bfs ctmc q i m (scan_f, p_classes, t0, limit, iter_f) = *)
(*   if not (Queue.is_empty q) then *)
(*     if i > limit then begin *)
(*       let stats = { *)
(*         t = (Unix.gettimeofday ()) -. t0; *)
(*         sim = infinity;  *)
(*         s = Hashtbl.length ctmc.v; *)
(*         r = Hashtbl.length ctmc.e; *)
(*         o = m; *)
(*       } in  *)
(*       raise (LIMIT (ctmc, stats)) *)
(*     end else begin  *)
(*       let (v, curr) = Queue.pop q in *)
(*       let ((new_s, old_s, i'), m') =  *)
(*         scan_f curr m ctmc i iter_f p_classes p_classes in *)
(*       (\* Add new states to v *\) *)
(*       let new_s' = fst (List.split new_s) in *)
(*       List.iter (fun (i, b) ->  *)
(*           Hashtbl.add ctmc.v (Big.key b) (i, b)) new_s'; *)
(*       (\* Add new states to q *\) *)
(*       List.iter (fun s -> Queue.push s q) new_s'; *)
(*       (\* Add labels for new states *\) *)
(*       (\* TO DO *\) *)
(*       (\* Add edges from v to new states *\) *)
(*       List.iter (fun ((u, _), rho) ->  *)
(*           Hashtbl.add ctmc.e v (u, rho)) new_s; *)
(*       (\* Add edges from v to old states *\) *)
(*       List.iter (fun (u, rho) -> Hashtbl.add ctmc.e v (u, rho)) old_s; *)
(*       (\* recursive call *\) *)
(*       _bfs ctmc q i' m' (scan_f, p_classes, t0, limit, iter_f) *)
(*     end  *)
(*   else *)
(*     let stats = { *)
(*       t = (Unix.gettimeofday ()) -. t0; *)
(*       sim = infinity;  *)
(*       s = Hashtbl.length ctmc.v; *)
(*       r = Hashtbl.length ctmc.e; *)
(*       o = m; *)
(*     } in  *)
(*     (ctmc, stats) *)

(* let _init_bfs s0 rewrite _scan srules limit ctmc_size iter_f = *)
(*   let consts =  *)
(*     (_scan, srules, Unix.gettimeofday (), limit, iter_f) *)
(*   and q = Queue.create () in *)
(*   (\* apply rewriting to s0 *\) *)
(*   let (s0', m) = rewrite s0 0 srules *)
(*   and ctmc = init_ctmc ctmc_size in *)
(*   Queue.push (0, s0') q; *)
(*   (\* add initial state *\) *)
(*   Hashtbl.add ctmc.v (Big.key s0') (0, s0'); *)
(*   (ctmc, s0', q, m, consts) *)

(* let bfs s0 srules limit ctmc_size iter_f = *)
(*   let (ctmc, s0', q, m, consts) = *)
(*     _init_bfs s0 rewrite _scan srules limit ctmc_size iter_f in *)
(*   iter_f 0 s0'; *)
(*   _bfs ctmc q 0 m consts     *)

(* let _init_sim s0 rewrite _scan srules t_max ctmc_size iter_f = *)
(*   Random.self_init (); *)
(*   let consts =  *)
(*     (_scan, srules, Unix.gettimeofday (), t_max, iter_f) in *)
(*   (\* apply rewriting to s0 *\) *)
(*   let (s0', m) = rewrite s0 0 srules *)
(*   and ctmc = init_ctmc ctmc_size in *)
(*   (\* add initial state *\) *)
(*   Hashtbl.add ctmc.v (Big.key s0') (0, s0'); *)
(*   (ctmc, s0', m, consts) *)

(* let rec _scan_sim (s : Big.bg) (m : int) iter_f pl pl_const =  *)
(*   match pl with *)
(*   | [] -> raise (DEAD m) *)
(*   | c :: cs -> *)
(*     begin *)
(*       match c with *)
(*       | P_class rr -> *)
(*         begin *)
(*           try *)
(*             let ((s', tau), m) = random_step s rr m in *)
(*             let (s'', m') = rewrite s' m pl_const in *)
(*             ((s'', tau), m') *)
(*           with *)
(*           | DEAD m -> (\* skip *\) *)
(*             _scan_sim s m iter_f cs pl_const *)
(*         end *)
(*       | P_rclass _ -> (\* skip *\) *)
(*         _scan_sim s m iter_f cs pl_const *)
(*     end *)

(* let rec _sim ctmc s t i m (scan_f, srules, t0, t_max, iter_f) = *)
(*   if t < t_max  *)
(*   then *)
(*     try *)
(*       let ((s', tau), m') =  *)
(*         scan_f s m iter_f srules srules in *)
(*       iter_f (i + 1) s'; *)
(*       Hashtbl.add ctmc.v (Big.key s') (i + 1, s'); *)
(*       Hashtbl.add ctmc.e i (i + 1, tau); *)
(*       _sim ctmc s' (t +. tau) (i + 1) m' (scan_f, srules, t0, t_max, iter_f) *)
(*     with *)
(*     | DEAD m -> (let stats = { *)
(*         t = (Unix.gettimeofday ()) -. t0; *)
(*         sim = t;  *)
(*         s = Hashtbl.length ctmc.v; *)
(*         r = Hashtbl.length ctmc.e; *)
(*         o = m; *)
(*       } in  *)
(*        (ctmc, stats)) *)
(*   else let stats = { *)
(*     t = (Unix.gettimeofday ()) -. t0; *)
(*     sim = t;  *)
(*     s = Hashtbl.length ctmc.v; *)
(*     r = Hashtbl.length ctmc.e; *)
(*     o = m; *)
(*   } in  *)
(*     raise (LIMIT (ctmc, stats)) *)

(* let sim s0 srules t_max ctmc_size iter_f = *)
(*   let (ctmc, s0', m, consts) = *)
(*     _init_sim s0 rewrite _scan_sim srules t_max ctmc_size iter_f in *)
(*   iter_f 0 s0'; *)
(*   _sim ctmc s0' 0.0 0 m consts     *)

(* let to_dot ctmc = *)
(*   let rank = "{ rank=source; 0 };\n" in *)
(*   let states = *)
(*     Hashtbl.fold (fun _ (i, _) buff ->  *)
(*         if i = 0 then Printf.sprintf  *)
(*             "%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \ *)
(*              fontname=\"monospace\", fixedsize=true, width=.60, height=.30 \ *)
(*              style=\"bold\" ];\n"  *)
(*             buff i i i i *)
(*         else Printf.sprintf  *)
(*             "%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \ *)
(*              fontname=\"monospace\", fixedsize=true, width=.60, height=.30 ];\n"  *)
(*             buff i i i i *)
(*       ) ctmc.v "" *)
(*   and edges = *)
(*     Hashtbl.fold (fun v (u, rho) buff ->  *)
(*         Printf.sprintf "%s%d -> %d [ label=\" %.3g\", fontname=\"monospace\", fontsize=7.0,\ *)
(*                  arrowhead=\"vee\", arrowsize=0.5 ];\n"  *)
(*           buff v u rho *)
(*       ) ctmc.e "" in *)
(*   Printf.sprintf "digraph ctmc {\nstylesheet = \"style_sbrs.css\"\n%s%s\n%s}" rank states edges *)

(* (\* sort *\) *)
(* let to_prism ctmc = *)
(*   let dims =  *)
(*     Printf.sprintf "%d %d\n" (Hashtbl.length ctmc.v) (Hashtbl.length ctmc.e) in *)
(*   Hashtbl.fold (fun v (u, rho) buff -> *)
(*       Printf.sprintf "%s%d %d %f\n" buff v u rho) ctmc.e dims *)

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
		   
(* let iter_states f ctmc = *)
(*   Hashtbl.iter (fun _ (i, b) -> f i b) ctmc.v *)
