open Big
open Format

(* functional react or sreact *)
type react = {
    rdx : bg;   (** Redex *)
    rct : bg;   (** Reactum *)
  }

type stats = {
  time : float;   (** Time *)
  states : int;   (** Non Iso states discoverd *)
  reacts : int;   (** Transitions *)
  occurs : int;   (** Total number of occurrences *)
}

module V = 
  Set.Make
    (struct 
      type t = int * bg
      let compare (v, a) (u, b) =
	match v - u with
	| 0 -> Big.compare a b
	| x -> x  
     end)

(*functional int or (int, float) for ctmc*)
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
| P_class_ide of string list  (** Priority class *)
| P_rclass_ide of string list (** Reducable priority class *)

(* raised when a state was already discovered *)
exception OLD of int

(* raised when the size of the ts reaches the limit *)
exception LIMIT of ts

(*CHECK with format*)
let string_of_react r =
  Printf.sprintf "%s\n->\n%s" 
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
  | P_class rs -> List.for_all (fun r -> is_valid_react r) rs
  | P_rclass rs -> List.for_all (fun r -> is_valid_react r) rs

let is_valid_p_ide get_react c =
  match c with
  | P_class_ide rs -> List.for_all (fun r -> is_valid_react (get_react r)) rs
  | P_rclass_ide rs -> List.for_all (fun r -> is_valid_react (get_react r)) rs

(* validity of each class is not checked *)
let is_valid_p_l l = List.exists (fun c ->
  match c with
  | P_class _ -> true
  | P_rclass _ -> false) l

let is_valid_p_ide_l get_react l = List.exists (fun c ->
  match c with
  | P_class_ide _ -> true
  | P_rclass_ide _ -> false) l

let is_react_enabled b r = occurs b r.rdx

let aux_apply (i_n, i_e) b r0 r1 =
  let (c, d, id) = decomp b r0 i_n i_e in
  comp c (comp (tens r1 id) d) 
  
(* Compute all the possible evolutions in one step. *)
(* No checks for solid redex *)
(* Iso states are merged *)
(* NODE_FREE may be raised by occurrences *)
let step s rules = 
  let filter_iso l =
    (List.fold_left (fun acc s ->
      if List.exists (fun a -> Big.equal a s) acc then acc
      else s :: acc) [] l), (List.length l) in
  filter_iso (List.fold_left (fun acc r ->
    (List.map (fun o ->
      aux_apply o s r.rdx r.rct) (occurrences s r.rdx)) @ acc) [] rules) 
    
let random_step s rules =
  let ss, l = step s rules in
  if (List.length ss) = 0 then raise NO_MATCH
  else List.nth ss (Random.int (List.length ss)), l

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
    let old = V.choose (V.filter (fun (_, old_b) ->
      Big.equal old_b b) v) in
    raise (OLD (fst old))
  with
  | Not_found -> true

(* Partition into new and old states *)
let _partition_aux ts i verb =
  List.fold_left (fun (new_acc, old_acc) b ->
    try 
      ignore (is_new b !ts.v);
      i := !i + 1;
      if verb then (printf "@[\r%d states found@?" (!i + 1)) else ();
      ((!i, b) :: new_acc, old_acc)
    with
    | OLD x -> (new_acc, x :: old_acc)
  ) ([], [])

(* Iterate over priority classes *)
let rec _scan step_f curr m ts i verb (l : p_class list) =
  match l with
  | [] -> [], []
  | c :: cs ->
    begin
      match c with
      | P_class rr ->
	begin
	  let ss, l = step_f curr rr in (* apply rewriting ? *)
	  if l = 0 then _scan step_f curr m ts i verb cs 
	  else (m := !m + l; _partition_aux ts i verb ss)
	end
      | P_rclass rr ->
	begin
	  try 
	    let b, l = fix curr rr in
	    m := !m + l;
	    _partition_aux ts i verb [b]
	  with
	  | NO_MATCH -> ([], [])
	end 	  
    end 

(* Iterate over priority classes *)
let rec _scan_ide get_react step_f curr m ts i verb (l : p_class_ide list) =
  match l with
  | [] -> [], []
  | c :: cs ->
    begin
      match c with
      | P_class_ide rr ->
	begin
	  let ss, l = step_f curr (List.map get_react rr) in (* apply rewriting ? *)
	  if l = 0 then _scan_ide get_react step_f curr m ts i verb cs 
	  else (m := !m + l; _partition_aux ts i verb ss)
	end
      | P_rclass_ide rr ->
	begin
	  try 
	    let b, l = fix curr (List.map get_react rr) in
	    m := !m + l;
	    _partition_aux ts i verb [b]
	  with
	  | NO_MATCH -> ([], [])
	end 	  
    end 

(* scan : a' list -> Big.bg list * int list *)
let _bfs s0 p_classes scan limit ts_size verb step_f =
  let t0 = Unix.gettimeofday ()
  and q = Queue.create () 
  and ts = ref 
    { v = V.singleton (0, s0);
      e = Hashtbl.create ts_size;
      l = Hashtbl.create ts_size; 
    } 
  and i = ref 0 
  and m = ref 1 in
  Queue.push (!i, s0) q;
  if verb then printf "@[1 state found@ " else ();
  while not (Queue.is_empty q) do
    if !i > limit then 
      (if verb then printf "in %f seconds.@]@." ((Unix.gettimeofday ()) -. t0) else (); 
       raise (LIMIT !ts))
    else 
      begin 
	let v, curr = Queue.pop q in
		
	let new_s, old_s = scan step_f curr m ts i verb p_classes in

	(* Add new states to v *)
	ts := 
	  { v = List.fold_left (fun acc s -> V.add s acc) !ts.v new_s;
	    e = !ts.e;
	    l = !ts.l; 
	  };

	(* Add new states to q *)
	List.iter (fun s -> Queue.push s q) new_s;

	(* Add labels for new states *)
	(* TO DO *)

	(* Add edges from v to new states *)
	List.iter (fun (u, _) -> Hashtbl.add !ts.e v u) new_s;

	(* Add edges from v to old states *)
	List.iter (fun u -> Hashtbl.add !ts.e v u) old_s;

      end
  done;
  let t = (Unix.gettimeofday ()) -. t0 in 
  if verb then printf "in %f seconds.@]@." t else ();
  (!ts, { time = t; 
	  states = V.cardinal !ts.v; 
	  reacts = Hashtbl.length !ts.e; 
	  occurs = !m })

let bfs s0 rules limit ts_size verb =
  if verb then (printf "@[Starting execution of BRS ...@]@.") else ();
  _bfs s0 rules _scan limit ts_size verb step

let bfs_ide s0 p_classes get_react limit ts_size verb =
  if verb then (printf "@[Starting execution of BRS ...@]@.") else ();
  _bfs s0 p_classes (_scan_ide get_react) limit ts_size verb step

let _sim_step x y =
  try
    let (s, l) = random_step x y in
    ([s], l)
  with
  | NO_MATCH -> ([], 0)

let sim s0 rules limit ts_size verb =
  if verb then (printf "@[Starting simulation of BRS ...@]@.") else ();
  _bfs s0 rules _scan limit ts_size verb _sim_step

let sim_ide s0 p_classes get_react limit ts_size verb =
  if verb then (printf "@[Starting simulation of BRS ...@]@.") else ();
  _bfs s0 p_classes (_scan_ide get_react) limit ts_size verb _sim_step

let to_dot ts =
  let states =
    String.concat "\n" (List.map (fun (i, _) -> 
      sprintf "%d [label=\"%d\" URL=\"./%d.svg\" fontsize=11.0];" i i i
    ) (V.elements ts.v))
  and edges =
    Hashtbl.fold (fun v u buff -> 
      sprintf "%s%d -> %d [arrowhead=\"vee\" arrowsize=0.5];\n" buff v u) ts.e "" in
  sprintf "digraph ts {\n%s\n%s}" states edges
 
let string_of_stats s =
  (sprintf "@\n@[===============================[ BRS Statistics ]================================@\n") ^
    (sprintf "|  Execution time (s)   : %-8g                                              |@\n" s.time) ^
    (sprintf "|  States               : %-8d                                              |@\n" s.states) ^
    (sprintf "|  Reactions            : %-8d                                              |@\n" s.reacts) ^
    (sprintf "|  Occurrences          : %-8d                                              |@\n" s.occurs) ^
    (sprintf "=================================================================================@]@\n")
