open Big
open Printf

type react = {
    rdx : bg;   (* Redex *)
    rct : bg;   (* Reactum *)
  }
  
type ts = {
  v : (bg_key, (int * bg)) Hashtbl.t;
  (* p : (int, Bilog) Hashtbl.t Predicates *)
  e : (int, int) Hashtbl.t;
  l : (int, int) Hashtbl.t;  
}


type stats = {
  t : float;  (** Execution time *)
  s : int;    (** Number of states *)
  r : int;    (** Number of reaction *)
  o : int;    (** Number of occurrences *)
}

type p_class = 
| P_class of react list 
| P_rclass of react list

type p_class_ide = 
| P_class_ide of string list  (* Standard priority class *)
| P_rclass_ide of string list (* Reducible priority class *)

(* raised when a state was already discovered *)
exception OLD of int

(* raised when the size of the ts reaches the limit *)
exception LIMIT of ts * stats

let init_ts n = 
  { v = Hashtbl.create n;
    e = Hashtbl.create n;
    l = Hashtbl.create n;
  }

let string_of_react r =
  sprintf "%s\n---->\n%s" 
    (string_of_bg r.rdx) (string_of_bg r.rct) 

let is_valid_react r =
  (inter_equal (inner r.rdx)  (inner r.rct)) 
  && (inter_equal (outer r.rdx) (outer r.rct))
  && (is_solid r.rdx)

(* Requirements for validity :
   - at least a standard class
   - every reaction has to be valid: solid and matching interfaces
 *)
let is_valid_p c =
  match c with
    | P_class rs | P_rclass rs -> 
      List.for_all (fun r -> is_valid_react r) rs
      && (List.length rs > 0)
  
let is_valid_p_ide get_react c =
  match c with
    | P_class_ide rs | P_rclass_ide rs ->
      List.for_all (fun r -> is_valid_react (get_react r)) rs
      && (List.length rs > 0)
	
(* validity of each class is not checked *)
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

let is_react_enabled b r = occurs b r.rdx

let rec is_class_enabled b rs = 
  match rs with
  | [] -> false
  | r :: rs ->
    if occurs b r.rdx then true
    else is_class_enabled b rs

let aux_apply (i_n, i_e) b r0 r1 =
  let (c, d, id) = decomp b r0 i_n i_e in
  comp c (comp (tens r1 id) d) 
  
(* Compute all the possible evolutions in one step. *)
(* No checks for solid redex *)
(* Iso states are merged *)
let step s rules = 
  let filter_iso l =
    ((List.fold_left (fun acc s ->
      if List.exists (fun a -> Big.equal a s) acc then acc
      else s :: acc) [] l), (List.length l)) in
  filter_iso (List.fold_left (fun acc r ->
    (List.map (fun o ->
      aux_apply o s r.rdx r.rct) (occurrences s r.rdx)) @ acc) [] rules) 
    
let random_step s rules =
  let ss, l = step s rules in
  if (List.length ss) = 0 then raise NO_MATCH
  else (List.nth ss (Random.int (List.length ss)), l)

(* Reduce a reducible class to the fixed point. Return the input state if no
   rewriting is performed. *)    
let fix s rules =
  let rec _step s rules =
    match rules with
    | [] -> raise NO_MATCH
    | r :: rs -> 
      begin
	try
	  (* just an occurrence in order to minimise the number of match
	     instances *)
	  aux_apply (occurrence s r.rdx) s r.rdx r.rct
	with
	| NO_MATCH -> _step s rs
      end in
  let rec _fix s rules i =
    try
      _fix (_step s rules) rules (i + 1)
    with
      | NO_MATCH -> (s, i) in
  _fix s rules 0

let is_new b v =
  let k = Big.key b in
  let k_buket = 
    Hashtbl.find_all v k in
  try 
    let (old, _) = List.find (fun (_, b') ->
      Big.equal b b') k_buket in
    raise (OLD old)
  with
  | Not_found -> true

(* Scan the piority classes and reduce a state. Stop when no more
   rules can be applied or when a non reducing piority class is
   enabled. *)
let rec rewrite s classes m =
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
 
let rec rewrite_ide get_react s classes m =
  match classes with
    | [] -> (s, m)
    | c :: cs ->
      begin
	match c with
	  | P_class_ide rr  ->
	    (* if there are matches then exit, skip otherwise *)
	    if is_class_enabled s (List.map get_react rr) then (s, m)
	    else rewrite_ide get_react s cs m
	  | P_rclass_ide rr ->
	    begin
	      let (s', i) = fix s (List.map get_react rr) in
	      rewrite_ide get_react s' cs (m + i)
	    end
      end

(* Partition a list of bigraphs into new and old states *)
let _partition_aux ts i f_iter =
  List.fold_left (fun (new_acc, old_acc, i) b ->
    try 
      ignore (is_new b ts.v);
      let i' = i + 1 in
      (* if iter_f then (printf "\r%6d states found%!" (i' + 1)); *)
      f_iter i' b;
      ((i', b) :: new_acc, old_acc, i')
    with
      | OLD x -> (new_acc, x :: old_acc, i)
  ) ([], [], i)
    
(* Iterate over priority classes *)
let rec _scan step_f curr m ts i iter_f (pl : p_class list) pl_const =
  match pl with
    | [] -> (([], [], i), m)
    | c :: cs ->
      begin
	match c with
	  | P_class rr ->
	    begin
	      let (ss, l) = step_f curr rr in
	      if l = 0 then _scan step_f curr m ts i iter_f cs pl_const 
	      else 
		(* apply rewriting *)
		let (ss', l') = 
		  List.fold_left (fun (ss,  l) s -> 
		    let (s', l') = rewrite s pl_const l in
		    (s' :: ss, l')) ([], l) ss in
		(_partition_aux ts i iter_f ss', m + l')
	    end
	  | P_rclass _ -> (* skip *)
	    _scan step_f curr m ts i iter_f cs pl_const
      end 

(* Iterate over priority classes *)
let rec _scan_ide get_react step_f curr m ts i iter_f pl pl_const =
  match pl with
    | [] -> (([], [], i), m)
  | c :: cs ->
    begin
      match c with
      | P_class_ide rr ->
	begin
	  let (ss, l) = step_f curr (List.map get_react rr) in
	  if l = 0 then 
	    _scan_ide get_react step_f curr m ts i iter_f cs pl_const 
	  else 
	    (* apply rewriting *)
	    let (ss', l') = 
	      List.fold_left (fun (ss,  l) s -> 
		let (s', l') = rewrite_ide get_react s pl_const l in
		(s' :: ss, l')) ([], l) ss in
	    (_partition_aux ts i iter_f ss', m + l')
	end
      | P_rclass_ide _ -> (* skip *)
	_scan_ide get_react step_f curr m ts i iter_f cs pl_const
    end 

(* queue contains already a state *) 
let rec _bfs ts q i m (scan_f, step_f, p_classes, t0, limit, ts_size, iter_f) =
  if not (Queue.is_empty q) then 
    if i > limit 
    then (let stats = 
	    { t = (Unix.gettimeofday ()) -. t0;
	      s = Hashtbl.length ts.v; 
	      r = Hashtbl.length ts.e;
	      o = m
	    } in
	  raise (LIMIT (ts, stats)))
    else begin 
      let (v, curr) = Queue.pop q in
      let ((new_s, old_s, i'), m') = 
	scan_f step_f curr m ts i iter_f p_classes p_classes in
      (* Add new states to v *)
      List.iter (fun (i, b) -> 
	Hashtbl.add ts.v (Big.key b) (i, b)) new_s;
      (* Add new states to q *)
      List.iter (fun s -> Queue.push s q) new_s;
      (* Add labels for new states *)
      (* TO DO *)
      (* Add edges from v to new states *)
      List.iter (fun (u, _) -> Hashtbl.add ts.e v u) new_s;
      (* Add edges from v to old states *)
      List.iter (fun u -> Hashtbl.add ts.e v u) old_s;
      (* recursive call *)
      _bfs ts q i' m' (scan_f, step_f, p_classes, t0, limit, ts_size, iter_f)
    end
  else let stats = 
	 { t = (Unix.gettimeofday ()) -. t0;
	   s = Hashtbl.length ts.v; 
	   r = Hashtbl.length ts.e;
	   o = m;
	 } in
       (ts, stats)

let _init_bfs s0 rewrite _scan step rules limit ts_size iter_f =
  let consts = 
    (_scan, step, rules, Unix.gettimeofday (), limit, ts_size, iter_f)
  and q = Queue.create () in
  (* apply rewriting to s0 *)
  let (s0', m) = rewrite s0 rules 0
  and ts = init_ts ts_size in
  Queue.push (0, s0') q;
  (* add initial state *)
  Hashtbl.add ts.v (Big.key s0') (0, s0');
  (ts, s0', q, m, consts)
 
let bfs s0 rules limit ts_size iter_f =
  let (ts, s0', q, m, consts) = 
    _init_bfs s0 rewrite _scan step rules limit ts_size iter_f in
  iter_f 0 s0';
  _bfs ts q 0 m consts    

let bfs_ide s0 p_classes get_react limit ts_size iter_f =
  let (ts, s0', q, m, consts) =
    _init_bfs 
      s0 (rewrite_ide get_react) (_scan_ide get_react) 
      step p_classes limit ts_size iter_f in
  iter_f 0 s0';
  _bfs ts q 0 m consts 

let _sim_step x y =
  try
    let (s, l) = random_step x y in
    ([s], l)
  with
  | NO_MATCH -> ([], 0)

let sim s0 rules limit ts_size iter_f =
  let (ts, s0', q, m, consts) =
    _init_bfs 
      s0 rewrite _scan _sim_step rules limit ts_size iter_f in
  iter_f 0 s0';
  _bfs ts q 0 m consts 

let sim_ide s0 p_classes get_react limit ts_size iter_f =
  let (ts, s0', q, m, consts) =
    _init_bfs s0 
      (rewrite_ide get_react) (_scan_ide get_react) _sim_step
      p_classes limit ts_size iter_f in
  iter_f 0 s0';
  _bfs ts q 0 m consts 

let string_of_stats s = 
  sprintf
    "\n\
     ===============================[ BRS Statistics ]===============================\n\
     |  Execution time (s)   : %-8.3g                                             |\n\
     |  States               : %-8d                                             |\n\
     |  Reactions            : %-8d                                             |\n\
     |  Occurrences          : %-8d                                             |\n\
     ================================================================================\n"
    s.t s.s s.r s.o

let to_dot ts =
  let states =
    Hashtbl.fold (fun _ (i, _) buff -> 
      if i = 0 then sprintf 
	"%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, \
                fontname=\"monospace\", fixedsize=true, width=.60, height=.30 \
                style=\"bold\" ];\n" 
	buff i i i
      else sprintf 
	"%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, \
                fontname=\"monospace\", fixedsize=true, width=.60, height=.30 ];\n" 
	buff i i i
    ) ts.v ""
  and edges =
    Hashtbl.fold (fun v u buff -> 
      sprintf "%s%d -> %d [ arrowhead=\"vee\", arrowsize=0.5, minlen=0.5 ];\n"
	buff v u) ts.e "" in
  sprintf "digraph ts {\n%s\n%s}" states edges

let to_prism ts =
  let dims = 
    sprintf "%d %d\n" (Hashtbl.length ts.v) (Hashtbl.length ts.e) in
  Hashtbl.fold (fun v u buff -> 
    sprintf "%s%d %d\n" buff v u) ts.e dims
  
let iter_states f ts =
  Hashtbl.iter (fun _ (i, b) -> f i b) ts.v
