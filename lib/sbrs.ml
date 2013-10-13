open Big
open Printf

type sreact = {
  rdx : bg;   (** Redex *)
  rct : bg;   (** Reactum *)
  rate : float; (** Rate *)
}

module V = Set.Make
  (struct 
    type t = int * bg
    let compare (v, a) (u, b) =
      match v - u with
	| 0 -> Big.compare a b
	| x -> x  
     end)

type ctmc = {
  v : V.t;
  (* p : (int, Bilog) Hashtbl.t Predicates *)
  e : (int, (int * float)) Hashtbl.t;
  l : (int, int) Hashtbl.t;  
}

type p_class = 
| P_class of sreact list 
| P_rclass of sreact list

type p_class_ide = 
| P_class_ide of string list  (** Priority class *)
| P_rclass_ide of string list (** Reducable priority class *)

(* raised when a state was already discovered *)
exception OLD of int

(* raised when the size of the ts reaches the limit *)
exception LIMIT of ctmc * (float * int * int * int)

exception LIMIT_SIM of ctmc * (float * float * int * int * int)

(* raised when a deadlock is reached in a simulation *)
exception DEAD of int

let init_ctmc n = 
  { v = V.empty;
    e = Hashtbl.create n;
    l = Hashtbl.create n;
  }

let string_of_sreact r =
  Printf.sprintf "%s\n--%g-->\n%s" 
    (string_of_bg r.rdx) r.rate (string_of_bg r.rct) 

let is_valid_sreact r =
  (inter_equal (inner r.rdx)  (inner r.rct)) 
  && (inter_equal (outer r.rdx) (outer r.rct)) 
  && (is_solid r.rdx) && (r.rate > 0.0)

let is_inst r =
  r.rate = infinity

let is_valid_p c =
  match c with
    | P_class rr -> (List.for_all is_valid_sreact rr)
      && (List.length rr > 0) 
      && (not (List.exists is_inst rr))
    | P_rclass rr -> (List.for_all is_valid_sreact rr)
      && (List.length rr > 0) 
      && (List.for_all is_inst rr)

let is_valid_p_ide get_sreact c =
  match c with
    | P_class_ide rr -> let rr' = List.map get_sreact rr in
			(List.for_all is_valid_sreact rr')
			&& (List.length rr' > 0) 
			&& (not (List.exists is_inst rr'))
    | P_rclass_ide rr -> let rr' = List.map get_sreact rr in
			 (List.for_all is_valid_sreact rr')
			 && (List.length rr' > 0) 
			 && (List.for_all is_inst rr')

let is_valid_p_l l = 
  List.exists (fun c ->
    match c with
      | P_class _ -> true
      | P_rclass _ -> false) l

let is_valid_p_ide_l l = 
  List.exists (fun c ->
    match c with
      | P_class_ide _ -> true
      | P_rclass_ide _ -> false) l

let is_sreact_enabled b r = occurs b r.rdx

let rec is_class_enabled b rs = 
  match rs with
  | [] -> false
  | r :: rs ->
    if occurs b r.rdx then true
    else is_class_enabled b rs

let aux_apply (i_n, i_e) b r0 r1 =
  let (c, d, id) = decomp b r0 i_n i_e in
  (*printf "c:\n%s\nd:\n%s\n\n" (string_of_bg c) (string_of_bg d);*)
  comp c (comp (tens r1 id) d)
  (* debug *)
  (*if occurs out r1 then out else failwith "BIG_INTERNAL_ERROR"*) 

let step s srules =
  let filter_iso l =
    ((List.fold_left (fun acc (s, rho) ->
      let (iso, non_iso) = 
	List.partition (fun (a, _) -> Big.equal a s) acc in
      match iso with
	| [] -> (s, rho) :: acc
	| [(a, lambda)] -> (a, lambda +. rho) :: non_iso
	(* impossible since acc has no duplicates *)
	| _ -> failwith "Sbrs.step") [] l), (List.length l)) in
  filter_iso (List.fold_left (fun acc r ->
    let occs = occurrences s r.rdx in 
    (List.map (fun o ->
      let s' = aux_apply o s r.rdx r.rct in
	(s', r.rate)) occs) @ acc) [] srules) 

(* rule selection: second step of SSA 
raise DEAD *)
let select_sreact (s : Big.bg) srules m =
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

(* Reduce a reducible (instantaneous) class to the fixed point. Return the input
   state if no rewriting is performed. *)    
let fix s srules =
  let rec _step s srules =
    match srules with
    | [] -> raise NO_MATCH
    | r :: rs -> 
      begin
	try
	  (* just an occurrence in order to minimise the number of match
	     instances *)
	  (*printf "s = %s\nrdx = %s\n" (string_of_bg s) (string_of_bg r.rdx);*)
	  aux_apply (occurrence s r.rdx) s r.rdx r.rct
	with
	| NO_MATCH -> _step s rs
      end in
  let rec _fix s srules i =
    try
      _fix (_step s srules) srules (i + 1)
    with
      | NO_MATCH -> (s, i) in
  _fix s srules 0

let is_new b v =
  try
    let old = V.choose (V.filter (fun (_, old_b) ->
      Big.equal old_b b) v) in
    raise (OLD (fst old))
  with
    | Not_found -> true

(* Scan the piority classes and reduce a state. Stop when no more
   rules can be applied or when a non reducing piority class is
   enabled. *)
let rec rewrite (s : Big.bg) classes (m : int) =
  match classes with
    | [] -> (s, m)
    | c :: cs ->
      begin
	match c with
	  | P_class rr  ->
	    (* if there are matches then exit, skip otherwise *)
	    if is_class_enabled s rr then (s, m)
	    else rewrite s cs m
	  | P_rclass rr ->
	    begin
	      let (s', i) = fix s rr in
	      rewrite s' cs (m + i)
	    end
      end 

let rec rewrite_ide get_sreact s classes m =
  match classes with
    | [] -> (s, m)
    | c :: cs ->
      begin
	match c with
	  | P_class_ide rr  ->
	    (* if there are matches then exit, skip otherwise *)
	    if is_class_enabled s (List.map get_sreact rr) then  (s, m)
	    else rewrite_ide get_sreact s cs m
	  | P_rclass_ide rr ->
	    begin
	      let (s', i) = fix s (List.map get_sreact rr) in
	      rewrite_ide get_sreact s' cs (m + i)
	    end
      end
      
(* Partition a list of bigraphs into new and old states *)
let _partition_aux ctmc i verb =
  List.fold_left (fun (new_acc, old_acc, i) (b, rho) ->
    try 
      ignore (is_new b ctmc.v);
      let i' = i + 1 in
      if verb then (printf "\r%d states found%!" (i' + 1));
      (((i', b), rho) :: new_acc, old_acc, i')
    with
      | OLD x -> (new_acc, (x, rho) :: old_acc, i)
  ) ([], [], i)

(* Iterate over priority classes *)
let rec _scan curr m ctmc i verb pl pl_const =
  match pl with
    | [] -> (([], [], i), m)
    | c :: cs ->
      begin
	match c with
	  | P_class rr ->
	    begin
	      let (ss, l) = step curr rr in
	      if l = 0 then _scan curr m ctmc i verb cs pl_const 
	      else 
		(* apply rewriting - instantaneous *)
		let (ss', l') = 
		  List.fold_left (fun (ss,  l) (s, rho) -> 
		    let (s', l') = rewrite s pl_const l in
		    ((s', rho) :: ss, l')) ([], l) ss in
		(_partition_aux ctmc i verb ss', m + l')
	    end
	  | P_rclass _ -> (* skip *)
	    _scan curr m ctmc i verb cs pl_const
      end 

let rec _scan_ide get_sreact curr m ctmc i verb pl pl_const =
  match pl with
  | [] -> (([], [], i), m)
  | c :: cs ->
    begin
      match c with
      | P_class_ide rr ->
	begin
	  let (ss, l) = step curr (List.map get_sreact rr) in
	  if l = 0 then _scan_ide get_sreact curr m ctmc i verb cs pl_const 
	  else begin 
	    (* apply rewriting - instantaneous *)
	    let (ss', l') = 
	      List.fold_left (fun (ss,  l) (s, rho) ->
		let (s', l') = rewrite_ide get_sreact s pl_const l in
		((s', rho) :: ss, l')) ([], l) ss in
	    (_partition_aux ctmc i verb ss', m + l')
	  end end
      | P_rclass_ide _ -> (* skip *)
	_scan_ide get_sreact curr m ctmc i verb cs pl_const 
    end 

let rec _bfs ctmc q i m (scan_f, p_classes, t0, limit, verb) =
  if not (Queue.is_empty q) then
    if i > limit then begin
      let t = (Unix.gettimeofday ()) -. t0 in 
      if verb then printf " in %f seconds.\n%!" t;
      raise (LIMIT (ctmc, (t, V.cardinal ctmc.v, Hashtbl.length ctmc.e, m)))
    end else begin 
      let (v, curr) = Queue.pop q in
      let ((new_s, old_s, i'), m') = 
	scan_f curr m ctmc i verb p_classes p_classes in
      (* Add new states to v *)
      let new_s' = fst (List.split new_s) in
      let ctmc' =
	{ v = List.fold_left (fun acc s -> V.add s acc) ctmc.v new_s';
	  e = ctmc.e;
	  l = ctmc.l; 
	} in
      (* Add new states to q *)
      List.iter (fun s -> Queue.push s q) new_s';
      (* Add labels for new states *)
      (* TO DO *)
      (* Add edges from v to new states *)
      List.iter (fun ((u, _), rho) -> 
	Hashtbl.add ctmc'.e v (u, rho)) new_s;
      (* Add edges from v to old states *)
      List.iter (fun (u, rho) -> Hashtbl.add ctmc'.e v (u, rho)) old_s;
      (* recursive call *)
      _bfs ctmc' q i' m' (scan_f, p_classes, t0, limit, verb)
    end 
  else begin
    let t = (Unix.gettimeofday ()) -. t0 in 
    if verb then printf " in %f seconds.\n%!" t;
    (ctmc, (t, V.cardinal ctmc.v, Hashtbl.length ctmc.e, m))
  end 

let _init_bfs s0 rewrite _scan srules limit ctmc_size verb =
  let consts = 
    (_scan, srules, Unix.gettimeofday (), limit, verb)
  and q = Queue.create () in
  (* apply rewriting to s0 *)
   let (s0', m) = rewrite s0 srules 0
  and ctmc = init_ctmc ctmc_size in
   Queue.push (0, s0') q;
  (* add initial state *)
  let ctmc' = 
    { v = V.add (0, s0') ctmc.v;
      e = ctmc.e;
      l = ctmc.l;
    } in
  (ctmc', q, m, consts)

let bfs s0 srules limit ctmc_size verb =
  if verb then (printf "Starting execution of SBRS ...\n1 state found");
  let (ctmc, q, m, consts) =
    _init_bfs s0 rewrite _scan srules limit ctmc_size verb in
   _bfs ctmc q 0 m consts    

let bfs_ide s0 p_classes get_sreact limit ctmc_size verb =
  if verb then (printf "Starting execution of SBRS ...\n1 state found");
  let (ctmc, q, m, consts) =
    _init_bfs s0 (rewrite_ide get_sreact) (_scan_ide get_sreact)
      p_classes limit ctmc_size verb in
  _bfs ctmc q 0 m consts 
    
let _init_sim s0 rewrite _scan srules t_max ctmc_size verb =
  let consts = 
    (_scan, srules, Unix.gettimeofday (), t_max, verb) in
  (* apply rewriting to s0 *)
  let (s0', m) = rewrite s0 srules 0
  and ctmc = init_ctmc ctmc_size in
  (* add initial state *)
  let ctmc' = 
    { v = V.add (0, s0') ctmc.v;
      e = ctmc.e;
      l = ctmc.l;
    } in
  (ctmc', s0', m, consts)

let rec _scan_sim (s : Big.bg) (m : int) verb pl pl_const = 
  match pl with
  | [] -> raise (DEAD m)
  | c :: cs ->
    begin
      match c with
      | P_class rr ->
	begin
	  try
	    let ((s', tau), m) = select_sreact s rr m in
	    let (s'', m') = rewrite s' pl_const m in
	    ((s'', tau), m')
	  with
	  | DEAD m -> (* skip *)
	    _scan_sim s m verb cs pl_const
	end
      | P_rclass _ -> (* skip *)
	_scan_sim s m verb cs pl_const
    end

let rec _scan_sim_ide get_sreact (s : Big.bg) (m : int) verb pl pl_const = 
  match pl with
  | [] -> raise (DEAD m)
  | c :: cs ->
    begin
      match c with
      | P_class_ide rr ->
	begin
	  try
	    let ((s', tau), m) = select_sreact s (List.map get_sreact rr) m in
	    let (s'', m') = rewrite_ide get_sreact  s' pl_const m in
	    ((s'', tau), m')
	  with
	  | DEAD m -> (* skip *)
	    _scan_sim_ide get_sreact s m verb cs pl_const
	end
      | P_rclass_ide _ -> (* skip *)
	_scan_sim_ide get_sreact s m verb cs pl_const
    end

let rec _sim ctmc s t i m (scan_f, srules, t0, t_max, verb) =
 if t < t_max then begin
   try
     let ((s', tau), m') = 
       scan_f s m verb srules srules in
     let ctmc' =
       { v = V.add (i + 1, s') ctmc.v;
	 e = ctmc.e;
	 l = ctmc.l;
       } in
     Hashtbl.add ctmc'.e i (i + 1, tau);
     _sim ctmc' s' (t +. tau) (i + 1) m' (scan_f, srules, t0, t_max, verb)
   with
   | DEAD m -> 
     begin
       let sim_t = (Unix.gettimeofday ()) -. t0 in 
       if verb then printf " in %f seconds.\n\
                            Deadlock at time %g.\n%!" sim_t t;
       (ctmc, (sim_t, t, V.cardinal ctmc.v, Hashtbl.length ctmc.e, m))
     end
 end else begin
   let sim_t = (Unix.gettimeofday ()) -. t0 in 
   if verb then printf " in %f seconds.\n%!" sim_t;
   raise (LIMIT_SIM (ctmc, (sim_t, t, V.cardinal ctmc.v, Hashtbl.length ctmc.e, m)))   
 end

let sim s0 srules t_max ctmc_size verb =
  if verb then (printf "Starting simulation of SBRS ...\n1 state found");
  let (ctmc, s0', m, consts) =
    _init_sim s0 rewrite _scan_sim srules t_max ctmc_size verb in
  _sim ctmc s0' 0.0 0 m consts    
  
let sim_ide s0 p_classes get_sreact t_max ctmc_size verb =
  if verb then (printf "Starting simulation of SBRS ...\n1 state found");
  let (ctmc, s0', m, consts) =
    _init_sim s0 (rewrite_ide get_sreact) (_scan_sim_ide get_sreact)
      p_classes t_max ctmc_size verb in
  _sim ctmc s0' 0.0 0 m consts    

let string_of_stats (t, s, r, o) = 
  sprintf
"\n===============================[ SBRS Statistics ]==============================\n\
   |  Execution time (s)   : %-8.3g                                             |\n\
   |  States               : %-8d                                             |\n\
   |  Reactions            : %-8d                                             |\n\
   |  Occurrences          : %-8d                                             |\n\
   ================================================================================\n"
     t s r o

let string_of_stats_sim (t, t_sim, s, r, o) = 
  sprintf
"\n===============================[ SBRS Statistics ]==============================\n\
   |  Execution time (s)   : %-8.3g                                             |\n\
   |  Simulation time      : %-8.3g                                             |\n\
   |  States               : %-8d                                             |\n\
   |  Reactions            : %-8d                                             |\n\
   |  Occurrences          : %-8d                                             |\n\
   ================================================================================\n"
     t t_sim s r o

let to_dot ctmc =
  let states =
    String.concat "\n" (List.map (fun (i, _) -> 
      if i = 0 then sprintf "%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, \
                    fontname=\"monospace\", fixedsize=true, width=.60, height=.30 \
                    style=\"bold\" ];" 
	i i i
      else sprintf "%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, \
                    fontname=\"monospace\", fixedsize=true, width=.60, height=.30 ];" 
	i i i
    ) (V.elements ctmc.v))
  and edges =
    Hashtbl.fold (fun v (u, rho) buff -> 
      sprintf "%s%d -> %d [ label=\" %.3g\", fontname=\"monospace\", fontsize=7.0,\
                            arrowhead=\"vee\", arrowsize=0.5, minlen=0.5 ];\n" 
	buff v u rho
    ) ctmc.e "" in
  sprintf "digraph ctmc {\n%s\n%s}" states edges

let to_prism ctmc =
  let dims = 
    sprintf "%d %d\n" (V.cardinal ctmc.v) (Hashtbl.length ctmc.e) in
  Hashtbl.fold (fun v (u, rho) buff ->
    sprintf "%s%d %d %f\n" buff v u rho) ctmc.e dims
