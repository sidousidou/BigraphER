type react =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option         (* Instantiation map *)
  }
    
include RrType.Make(
	    struct
	      type t = react	
	      type label = Epsilon               (* Empty label       *)  
	      type occ = Big.bg

	      let lhs r = r.rdx
	      let rhs r = r.rct
	      let l _ = Epsilon
	      let map r = r.eta
	      let string_of_label _ = ""
 	      let val_chk _ = true
	      let to_occ b _ = b
	      let big_of_occ b = b
	      let merge_occ b _ = b
	    end)

type ts = {
    v : (Big.bg_key, (int * Big.bg)) Hashtbl.t;
    e : (int, int) Hashtbl.t;
    l : (int, int) Hashtbl.t;
  }

type stats = {
    t : float;  (** Execution time *)
    s : int;    (** Number of states *)
    r : int;    (** Number of reaction *)
    o : int;    (** Number of occurrences *)
  }

(* include TsType.Make(struct *)
(* 			type t = *)
(* 			  { v : (bg_key, (int * bg)) Hashtbl.t; *)
(* 			    (\* p : (int, Bilog) Hashtbl.t Predicates *\) *)
(* 			    e : (int, int) Hashtbl.t; *)
(* 			    l : (int, int) Hashtbl.t; } *)
(* 			type stats = *)
(* 			  { t : float;   (\* Execution time *\) *)
(* 			    s : int;     (\* Number of states *\) *)
(* 			    r : int;     (\* Number of reaction *\) *)
(* 			    o : int; }   (\* Number of occurrences *\) *)
			
(* 		      end)		     *)

	       
type p_class = 
| P_class of react list 
| P_rclass of react list

(* raised when a state was already discovered *)
exception OLD of int

(* raised when the size of the ts reaches the limit *)
exception LIMIT of ts * stats

(* remove element with index i *)
let rec aux i i' acc = function
  | [] -> (None, acc)
  | x :: l -> if i = i' then (Some x, l @ acc)
	      else aux i (i' + 1) (x :: acc) l
			  
(* - Select a reaction rule randomly
   - Try to rewrite
   - Done on success, repeat with the remainig reaction rules otherwise *)			  
let random_step b rules =
  let t = Sparse.trans b.Big.p.Place.nn in
  let rec _random_step b = function
    | [] -> None
    | rs ->
       (let (r, rs') =
	  aux (Random.int (List.length rs)) 0 [] rs in
	match r with
	| None -> assert false
	| Some r ->
	   (match Big.occurrence b (lhs r) t with
	    | Some o ->
	       Some (Big.rewrite o b (lhs r) (rhs r) (map r))
	    | None -> _random_step b rs')) in
  _random_step b rules
			  
let init_ts n = 
  { v = Hashtbl.create n;
    e = Hashtbl.create n;
    l = Hashtbl.create n;
  }

(* Requirements for validity :
   - at least a standard class
   - every reaction has to be valid: solid and matching interfaces
 *)
let is_valid_p c =
  match c with
    | P_class rs | P_rclass rs -> 
      List.for_all is_valid rs
      && (List.length rs > 0)
  	
(* validity of each class is not checked *)
let is_valid_p_l l = 
  List.exists (fun c ->
    match c with
      | P_class _ -> true
      | P_rclass _ -> false) l

let rec is_class_enabled b = function 
  | [] -> false
  | r :: rs ->
     if Big.occurs b (lhs r) then true
     else is_class_enabled b rs
  

let is_new b v =
  let k = Big.key b in
  let k_buket = 
    Hashtbl.find_all v k in
  try 
    let (old, _) = List.find (fun (_, b') ->
        (* printf "b =? b'\n\ *)
        (*         ----------------\n\ *)
        (*         %s\n\ *)
        (*         ----------------\n\ *)
        (*         %s\n\ *)
        (*         ----------------\n" (Big.to_string b) (Big.to_string b'); *)
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
 
(* Partition a list of bigraphs into new and old states *)
let _partition_aux ts i f_iter =
  List.fold_left (fun (new_acc, old_acc, i) b ->
    try 
      ignore (is_new b ts.v);
      let i' = i + 1 in
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
		  (s' :: ss, l')
                ) ([], l) ss in
	    (_partition_aux ts i iter_f ss', m + l')
	end
      | P_rclass _ -> (* skip *)
	_scan step_f curr m ts i iter_f cs pl_const
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

let _sim_step b rules =
  match random_step b rules with
  | Some b -> ([b], 1)
  | None -> ([], 0)

let sim s0 rules limit ts_size iter_f =
  Random.self_init ();
  let (ts, s0', q, m, consts) =
    _init_bfs 
      s0 rewrite _scan _sim_step rules limit ts_size iter_f in
  iter_f 0 s0';
  _bfs ts q 0 m consts 

let to_dot ts =
  let rank = "{ rank=source; 0 };\n" in
  let states =
    Hashtbl.fold (fun _ (i, _) buff -> 
      if i = 0 then Printf.sprintf 
	"%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \
                fontname=\"monospace\", fixedsize=true, width=.60, height=.30 \
                style=\"bold\" ];\n" 
	buff i i i i
      else Printf.sprintf 
	"%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \
                fontname=\"monospace\", fixedsize=true, width=.60, height=.30 ];\n" 
	buff i i i i
    ) ts.v ""
  and edges =
    Hashtbl.fold (fun v u buff -> 
      Printf.sprintf "%s%d -> %d [ arrowhead=\"vee\", arrowsize=0.5 ];\n"
	buff v u) ts.e "" in
  Printf.sprintf "digraph ts {\nstylesheet = \"style_brs.css\"\n%s%s\n%s}" rank states edges

let to_prism ts =
  let dims =
    (Hashtbl.length ts.v, Hashtbl.length ts.e)
  and edges =
    Hashtbl.fold (fun v u acc ->
		  (v, u) :: acc) ts.e [] in
  dims :: (List.fast_sort Base.ints_compare edges)
  |> List.map (fun (v, u) ->
	       (string_of_int v) ^ " " ^ (string_of_int u))
  |> String.concat "\n"
  
let iter_states f ts =
  Hashtbl.iter (fun _ (i, b) -> f i b) ts.v
