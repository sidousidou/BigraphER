open Big
open Printf

type react = {
    rdx : bg;   (* Redex *)
    rct : bg;   (* Reactum *)
  }

module V = Set.Make
  (struct 
    type t = int * bg
    let compare (v, a) (u, b) =
      match v - u with
	| 0 -> Big.compare a b
	| x -> x  
     end)
  
type ts = {
  v : V.t;
  (* p : (int, Bilog) Hashtbl.t Predicates *)
  e : (int, int) Hashtbl.t;
  l : (int, int) Hashtbl.t;  
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
exception LIMIT of ts

let init_ts n = 
  { v = V.empty;
    e = Hashtbl.create n;
    l = Hashtbl.create n;
  }

let string_of_react r =
  Printf.sprintf "%s\n---->\n%s" 
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
  try
    let old = V.choose (V.filter (fun (_, old_b) ->
      Big.equal old_b b) v) in
    raise (OLD (fst old))
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
let _partition_aux ts i verb =
  List.fold_left (fun (new_acc, old_acc, i) b ->
    try 
      ignore (is_new b ts.v);
      let i' = i + 1 in
      if verb then (printf "\r%d states found%!" (i' + 1));
      ((i', b) :: new_acc, old_acc, i')
    with
      | OLD x -> (new_acc, x :: old_acc, i)
  ) ([], [], i)
    
(* Iterate over priority classes *)
let rec _scan step_f curr m ts i verb (pl : p_class list) pl_const =
  match pl with
    | [] -> (([], [], i), m)
    | c :: cs ->
      begin
	match c with
	  | P_class rr ->
	    begin
	      let (ss, l) = step_f curr rr in
	      if l = 0 then _scan step_f curr m ts i verb cs pl_const 
	      else 
		(* apply rewriting *)
		let (ss', l') = 
		  List.fold_left (fun (ss,  l) s -> 
		    let (s', l') = rewrite s pl_const l in
		    (s' :: ss, l')) ([], l) ss in
		(_partition_aux ts i verb ss', m + l')
	    end
	  | P_rclass _ -> (* skip *)
	    _scan step_f curr m ts i verb cs pl_const
      end 

(* Iterate over priority classes *)
let rec _scan_ide get_react step_f curr m ts i verb pl pl_const =
  match pl with
    | [] -> (([], [], i), m)
  | c :: cs ->
    begin
      match c with
      | P_class_ide rr ->
	begin
	  let (ss, l) = step_f curr (List.map get_react rr) in
	  if l = 0 then 
	    _scan_ide get_react step_f curr m ts i verb cs pl_const 
	  else 
	    (* apply rewriting *)
	    let (ss', l') = 
	      List.fold_left (fun (ss,  l) s -> 
		let (s', l') = rewrite_ide get_react s pl_const l in
		(s' :: ss, l')) ([], l) ss in
	    (_partition_aux ts i verb ss', m + l')
	end
      | P_rclass_ide _ -> (* skip *)
	_scan_ide get_react step_f curr m ts i verb cs pl_const
    end 

(* queue contains already a state *) 
let rec _bfs ts q i m (scan_f, step_f, p_classes, t0, limit, ts_size, verb) =
  if not (Queue.is_empty q) then
    if i > limit then begin 
      if verb then printf " in %f seconds.\n%!" ((Unix.gettimeofday ()) -. t0);
      raise (LIMIT ts)
    end
    else begin 
      let (v, curr) = Queue.pop q in
      let ((new_s, old_s, i'), m') = 
	scan_f step_f curr m ts i verb p_classes p_classes in
      (* Add new states to v *)
      let ts' =
	{ v = List.fold_left (fun acc s -> V.add s acc) ts.v new_s;
	  e = ts.e;
	  l = ts.l; 
	} in
      (* Add new states to q *)
      List.iter (fun s -> Queue.push s q) new_s;
      (* Add labels for new states *)
      (* TO DO *)
      (* Add edges from v to new states *)
      List.iter (fun (u, _) -> Hashtbl.add ts'.e v u) new_s;
      (* Add edges from v to old states *)
      List.iter (fun u -> Hashtbl.add ts'.e v u) old_s;
      (* recursive call *)
      _bfs ts' q i' m' (scan_f, step_f, p_classes, t0, limit, ts_size, verb)
    end
  else begin
    let t = (Unix.gettimeofday ()) -. t0 in 
    if verb then printf " in %f seconds.\n%!" t;
    (ts, (t, V.cardinal ts.v, Hashtbl.length ts.e, m))
  end 

let _init_bfs s0 rewrite _scan step rules limit ts_size verb =
  let consts = 
    (_scan, step, rules, Unix.gettimeofday (), limit, ts_size, verb)
  and q = Queue.create () in
  (* apply rewriting to s0 *)
  let (s0', m) = rewrite s0 rules 0
  and ts = init_ts ts_size in
  Queue.push (0, s0') q;
  (* add initial state *)
  let ts' = 
    { v = V.add (0, s0') ts.v;
      e = ts.e;
      l = ts.l;
    } in
  (ts', q, m, consts)
 
let bfs s0 rules limit ts_size verb =
  if verb then (printf "Starting execution of BRS ...\n1 state found");
  let (ts, q, m, consts) = 
    _init_bfs s0 rewrite _scan step rules limit ts_size verb in
  _bfs ts q 0 m consts    

let bfs_ide s0 p_classes get_react limit ts_size verb =
  if verb then (printf "Starting execution of BRS ...\n1 state found");
  let (ts, q, m, consts) =
    _init_bfs s0 (rewrite_ide get_react) (_scan_ide get_react) step p_classes limit ts_size verb in
  _bfs ts q 0 m consts 

let _sim_step x y =
  try
    let (s, l) = random_step x y in
    ([s], l)
  with
  | NO_MATCH -> ([], 0)

let sim s0 rules limit ts_size verb =
  if verb then (printf "Starting simulation of BRS ...\n1 state found");
  let (ts, q, m, consts) =
    _init_bfs s0 rewrite _scan _sim_step rules limit ts_size verb in
  _bfs ts q 0 m consts 

let sim_ide s0 p_classes get_react limit ts_size verb =
  if verb then (printf "Starting simulation of BRS ...\n1 state found");
  let (ts, q, m, consts) =
    _init_bfs s0 (rewrite_ide get_react) (_scan_ide get_react) _sim_step
      p_classes limit ts_size verb in
  _bfs ts q 0 m consts 

let string_of_stats (t, s, r, o) = 
  sprintf
"\n===============================[ BRS Statistics ]===============================\n\
   |  Execution time (s)   : %-8g                                             |\n\
   |  States               : %-8d                                             |\n\
   |  Reactions            : %-8d                                             |\n\
   |  Occurrences          : %-8d                                             |\n\
   ================================================================================\n"
     t s r o

let to_dot ts =
  let states =
    String.concat "\n" (List.map (fun (i, _) -> 
      sprintf "%d [label=\"%d\", URL=\"./%d.svg\", fontsize=11.0, \
               fontnames=\"ps\"];" i i i) (V.elements ts.v))
  and edges =
    Hashtbl.fold (fun v u buff -> 
      sprintf "%s%d -> %d [arrowhead=\"vee\", arrowsize=0.5];\n"
	buff v u) ts.e "" in
  sprintf "digraph ts {\n%s\n%s}" states edges
