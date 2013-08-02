open Big
open Printf

type sreact = {
  rdx : bg;   (** Redex *)
  rct : bg;   (** Reactum *)
  rate : float; (** Rate *)
}

type ctmc = {
  v : Brs.V.t;
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
exception LIMIT of ctmc

let string_of_sreact r =
  Printf.sprintf "%s\n-%g->\n%s" 
    (string_of_bg r.rdx) r.rate (string_of_bg r.rct) 

let is_valid_sreact r =
  (inter_equal (inner r.rdx)  (inner r.rct)) &&
  (inter_equal (outer r.rdx) (outer r.rct)) &&
  (is_solid r.rdx) && (r.rate > 0.0)

(* At least a non-reducing class *)
(* if one inst in a class then all inst*)
(* CHECK *)
let is_valid_p l =
  (List.exists (fun c ->
    match c with
    | P_class _ -> true
    | P_rclass _ -> false) l) &&
    (List.for_all (fun c ->
      match c with
      | P_class r -> (List.length r > 0) && (List.for_all is_valid_sreact r)
      | P_rclass r -> (List.length r > 0) && (List.for_all is_valid_sreact r)) l)

(* CHECK *) 
let is_valid_p_ide get_sreact c =
  let _aux r =
    (List.length r > 0)
    && (let (inst, stoch) = List.partition (fun r ->
      r.rate = infinity) (List.map get_sreact r) in
	match inst with
    | [] -> true
    | _ -> match stoch with
      | [] -> true
      | _ -> false)
    && (List.for_all (fun r -> is_valid_sreact (get_sreact r)) r) in
  match c with
  | P_class_ide r -> _aux r
  | P_rclass_ide r -> (List.exists (fun r -> 
    (get_sreact r).rate = infinity) r) && (_aux r)

let is_react_enabled b r = occurs b r.rdx

let aux_apply (i_n, i_e) b r0 r1 =
  let (c, d, id) = decomp b r0 i_n i_e in
  comp c (comp (tens r1 id) d) 

let step s srules = 
  let filter_iso l =
    (List.fold_left (fun acc (s, rho) ->
      let (iso, non_iso) = 
	List.partition (fun (a, _) -> Big.equal a s) acc in
      match iso with
      (* Other matches are impossible since acc has no duplicates *)
      | [] -> (s, rho) :: acc
      | [(a, lambda)] -> (a, lambda +. rho) :: non_iso) [] l), (List.length l) in
  filter_iso (List.fold_left (fun acc r ->
    (List.map (fun o ->
      aux_apply o s r.rdx r.rct, r.rate) (occurrences s r.rdx)) @ acc) [] srules) 

let fix s rules =
  let rec _step s rules =
    match rules with
    | [] -> raise NO_MATCH
    | r :: rs -> 
      begin
	try
	  aux_apply (occurrence s r.rdx) s r.rdx r.rct
	with
	| NO_MATCH -> _step s rs
      end in
  let rec _fix s rules i =
    try
      _fix (_step s rules) rules (i + 1)
    with
    | NO_MATCH -> s, i in
  _fix (_step s rules) rules 1

let is_new b v =
  try
    let old = Brs.V.choose (Brs.V.filter (fun (_, old_b) ->
      Big.equal old_b b) v) in
    raise (OLD (fst old))
  with
  | Not_found -> true

(* Partition into new and old states *)
let _partition_aux ctmc i verb =
  List.fold_left (fun (new_acc, old_acc) (b, rho) ->
    try 
      ignore (is_new b !ctmc.v);
      i := !i + 1;
      if verb then (printf "\r%d states found " (!i + 1); flush_all ()) else ();
      (((!i, b), rho) :: new_acc, old_acc)
    with
    | OLD x -> (new_acc, (x, rho) :: old_acc)
  ) ([], [])

(* Iterate over priority classes *)
let rec _scan step_f curr m ctmc i verb (l : p_class list) =
  match l with
  | [] -> [], []
  | c :: cs ->
    begin
      match c with
      | P_class rr ->
	begin
	  let (ss : (Big.bg * float) list), l = step_f curr rr in (* apply rewriting ? *)
	  if l = 0 then _scan step_f curr m ctmc i verb cs 
	  else (m := !m + l; _partition_aux ctmc i verb ss)
	end
      | P_rclass rr -> (* instantaneous reactions *)
	begin
	  try 
	    let b, l = fix curr rr in
	    m := !m + l;
	    _partition_aux ctmc i verb [(b, infinity)]
	  with
	  | NO_MATCH -> ([], [])
	end 	  
    end 

(* Iterate over priority classes *)
let rec _scan_ide get_sreact step_f curr m ctmc i verb (l : p_class_ide list) =
  match l with
  | [] -> [], []
  | c :: cs ->
    begin
      match c with
      | P_class_ide rr ->
	begin
	  let ss, l = step_f curr (List.map get_sreact rr) in (* apply rewriting ? *)
	  if l = 0 then _scan_ide get_sreact step_f curr m ctmc i verb cs 
	  else (m := !m + l; _partition_aux ctmc i verb ss)
	end
      | P_rclass_ide rr ->
	begin
	  try 
	    let b, l = fix curr (List.map get_sreact rr) in
	    m := !m + l;
	    _partition_aux ctmc i verb [(b, infinity)]
	  with
	  | NO_MATCH -> ([], [])
	end 	  
    end 

let _bfs s0 p_classes scan limit ctmc_size verb step_f =
  let t0 = Unix.gettimeofday ()
  and q = Queue.create () 
  and ctmc = ref 
    { v = Brs.V.singleton (0, s0);
      e = Hashtbl.create ctmc_size;
      l = Hashtbl.create ctmc_size; 
    } 
  and i = ref 0 
  and m = ref 1 in
  Queue.push (!i, s0) q;
  if verb then printf "1 state found " else ();
  while not (Queue.is_empty q) do
    if !i > limit then 
      (if verb then printf "in %f seconds.\n" ((Unix.gettimeofday ()) -. t0) else (); 
       raise (LIMIT !ctmc))
    else 
      begin 
	let v, curr = Queue.pop q in
		
	let (new_s : ((int * Big.bg) * float) list), (old_s : (int  * float) list) = 
	  scan step_f curr m ctmc i verb p_classes in
	let new_s_V = fst (List.split new_s) in
	
	(* Add new states to v *)
	ctmc := 
	  { v = List.fold_left (fun acc s -> Brs.V.add s acc) !ctmc.v new_s_V;
	    e = !ctmc.e;
	    l = !ctmc.l; 
	  };

	(* Add new states to q *)
	List.iter (fun s -> Queue.push s q) new_s_V;

	(* Add labels for new states *)
	(* TO DO *)

	(* Add edges from v to new states *)
	List.iter (fun ((u, _), rho) -> Hashtbl.add !ctmc.e v (u, rho)) new_s;

	(* Add edges from v to old states *)
	List.iter (fun (u, rho) -> Hashtbl.add !ctmc.e v (u, rho)) old_s;

      end
  done;
  let t = (Unix.gettimeofday ()) -. t0 in 
  if verb then printf "in %f seconds.\n" t else ();
  (!ctmc, { Brs.time = t; 
	    states = Brs.V.cardinal !ctmc.v; 
	    reacts = Hashtbl.length !ctmc.e; 
	    occurs = !m })

let bfs s0 rules limit ctmc_size verb =
  if verb then (printf "Starting execution of CTMC ...\n"; flush_all ()) else ();
  _bfs s0 rules _scan limit ctmc_size verb step

let bfs_ide s0 p_classes get_sreact limit ctmc_size verb =
  if verb then (printf "Starting execution of CTMC ...\n"; flush_all ()) else ();
  _bfs s0 p_classes (_scan_ide get_sreact) limit ctmc_size verb step

let to_dot ctmc =
  let states =
    String.concat "\n" (List.map (fun (i, _) -> 
      sprintf "%d [label=\"%d\" URL=\"./%d.svg\" fontsize=11.0];" i i i
    ) (Brs.V.elements ctmc.v))
  and edges =
    Hashtbl.fold (fun v (u, rho) buff -> 
      sprintf "%s%d -> %d [label=\"%g\" arrowhead=\"vee\" arrowsize=0.5];\n" buff v u rho) ctmc.e "" in
  sprintf "digraph ctmc {\n%s\n%s}" states edges
