open Printf
open Base

(* Type for concrete place graphs. The elements are roots, nodes, sites and
   for adjacency matrices: roots to nodes, roots to sites, nodes to nodes and
   nodes to sites. *)
type pg = { r: int; 
	    n: int; 
	    s: int; 
	    rn: Sparse.bmatrix; 
	    rs: Sparse.bmatrix; 
	    nn: Sparse.bmatrix;
	    ns: Sparse.bmatrix;
	  }

(* Raised by comp. The elements are (sites, roots) *)
exception COMP_ERROR of (int * int) 

(* String representation *)
let to_string p =
  let m = Sparse.stack (Sparse.append p.rn p.rs) (Sparse.append p.nn p.ns) in
  sprintf "%d %d %d\n%s\n" p.r p.n p.s (Sparse.to_string m)

(* Parse a place graph from a list of strings *)
let parse r n s lines =
  assert (r >= 0);
  assert (n >= 0);
  assert (s >= 0);
  assert (List.length lines = r + n);
  assert (List.for_all (fun l -> String.length l = n + s) lines);
  let rn = Sparse.make r n
  and rs = Sparse.make r s
  and nn = Sparse.make n n
  and ns = Sparse.make n s in
  ignore (List.fold_left (fun i line ->
    if i < r then begin
      (* roots - nodes *)
      for j = 0 to n - 1 do
	if line.[j] = '1' then Sparse.add rn i j
      done;
      (* roots - sites *)
      for j = n to n + s - 1 do
	if line.[j] = '1' then Sparse.add rs i (j - n)
      done
    end else begin
      (* nodes - nodes *)
      for j = 0 to n - 1 do
	if line.[j] = '1' then Sparse.add nn (i - r) j
      done;
      (* nodes - sites *)
      for j = n to n + s - 1 do
	if line.[j] = '1' then Sparse.add ns (i -r) (j - n)
      done
    end;
    i + 1) 0 lines);
  { r = r;
    n = n;
    s = s;
    rn = rn;
    rs = rs;
    nn = nn;
    ns = ns;
  }

(* Apply isomorphism *)  
let apply_iso i p =
  { p with 
    rn = Sparse.apply_iso_cols i p.rn;
    nn = Sparse.apply_iso i p.nn;
    ns = Sparse.apply_iso_rows i p.ns;
  }

(* Elementary place graphs *)
let elementary_id n =
  assert (n >= 0);
  { r = n;
    n = 0;
    s = n;
    rn = Sparse.make n 0;
    rs = Sparse.diag n; 
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 n;
  }
    
let elementary_merge n =
  assert (n >= 0);
  { r = 1;
    n = 0;
    s = n;
    rn = Sparse.make 1 0;
    rs = Sparse.row_1 n; 
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 n;
  }
    
let elementary_split n =
  assert (n >= 0);
  { r = n;
    n = 0;
    s = 1;
    rn = Sparse.make n 0;
    rs = Sparse.col_1 n; 
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 1;
  }

let id0 = elementary_id 0

let one = elementary_merge 0

let zero = elementary_split 0

let elementary_sym m n =
  assert (m >= 0);
  assert (n >= 0);
  { r = m + n;
    n = 0;
    s = m + n;
    rn = Sparse.make (m + n) 0;
    rs = Sparse.stack 
      (Sparse.append (Sparse.make n m) (Sparse.diag n)) 
      (Sparse.append (Sparse.diag m) (Sparse.make m n)); 
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 (m + n);
  }
    
let elementary_ion =
  { r = 1;
    n = 1;
    s = 1;
    rn = Sparse.row_1 1;
    rs = Sparse.make 1 1;
    nn = Sparse.make 1 1;
    ns = Sparse.row_1 1;
  }

(* Parse a placing *)
let parse_placing l r =
  assert (r >= 0);
  { r = r;
    n = 0; 
    s = List.length l;
    rn = Sparse.make r 0;
    rs = Sparse.parse_vector l r;
    nn = Sparse.make 0 0;      
    ns = Sparse.make 0 (List.length l);
  }

(* placing equality *)
let equal_placing a b =
  assert (a.n = 0);  
  assert (b.n = 0);
  (a.r = b.r) && (a.s = b.s) && (Sparse.(=) a.rs b.rs)

(* placing compare *)  
let compare_placing a b =
  assert (a.n = 0);
  assert (b.n = 0);
  let x = a.r - b.r in
  match x with
    | 0 -> (let x = a.s - b.s in
	    match x with
	      | 0 -> Sparse.compare a.rs b.rs
	      | _ -> x)
    | _ -> x

(* Tensor product: A x B (indices in the right hand-side are increased) *)
let tens a b =
  { r = a.r + b.r;
    n = a.n + b.n;
    s = a.s + b.s;
    rn = Sparse.tens a.rn b.rn;
    rs = Sparse.tens a.rs b.rs;
    nn = Sparse.tens a.nn b.nn;
    ns = Sparse.tens a.ns b.ns;
  }
    
(* Composition: G o F (indices in the right hand-side are increased) *)
let comp g f =
  if g.s = f.r then 
    { r = g.r;
      n = g.n + f.n;
      s = f.s;
      rn = Sparse.append g.rn (Sparse.mul g.rs f.rn);
      rs = Sparse.mul g.rs f.rs;
      nn = Sparse.stack 
	(Sparse.append g.nn (Sparse.mul g.ns f.rn)) 
	(Sparse.append (Sparse.make f.n g.n) f.nn);
      ns = Sparse.stack (Sparse.mul g.ns f.rs) f.ns;
    }
  else raise (COMP_ERROR (g.s, f.r))
    
(* Is p an identity? *)
let is_id p =
  match p with
  | { r = x; n = 0; _ } -> p.rs = Sparse.diag x
  | _ -> false

let is_plc p = 
  p.n = 0
  
(* Is p monomorphic?: no two sites are siblings and no site is an orphan *)
let is_mono p =
  let slice = Sparse.stack p.rs p.ns in
  if IntSet.is_empty (Sparse.orphans slice) then Sparse.siblings_chk slice
  else false
    
(* Is p epimorphic: no root is idle and no two roots are partners *)
let is_epi p =
  let slice = Sparse.append p.rn p.rs in
  if IntSet.is_empty (Sparse.leaves slice) then Sparse.partners_chk slice
  else false
    
(* Is p guarded: no root has sites as children *)
let is_guard p = 
  (Sparse.entries p.rs) = 0
 
(* Build the decomposition of target t given pattern p and isomorphism over
   nodes i: p -> t. The result is context c, id, d, and nodes in c and d 
   expressed as rows of t. Pattern p is mono and epi.
   See page 76, proposition 4.2.4. *)
let decomp t p iso =
  let trans_t_nn = Sparse.trans t.nn
  and iso' = Iso.inverse iso 
  and v_p' = IntSet.of_list (Iso.codom iso) in 
  let v_c = 
    IntSet.diff 
      (IntSet.fold (fun i acc ->
	IntSet.union acc (IntSet.of_list (Sparse.prn trans_t_nn i)))
	 v_p' IntSet.empty) v_p' in
  let v_d = IntSet.diff (IntSet.of_int t.n) (IntSet.union v_c v_p') in
  (* fix numbering of nodes in c and d : t -> c and t -> d *)
  let iso_v_c = IntSet.fix v_c 
  and iso_v_d = IntSet.fix v_d in
  (* c roots to d nodes *)
  let (edg_c_rs0, edg_d_rn0, s0) = 
    IntSet.fold (fun r acc ->
      List.fold_left (fun (acc_c, acc_d, s) c ->
	if IntSet.mem c v_d then 
	  ((r, s) :: acc_c, (s, Iso.find iso_v_d c) :: acc_d, s + 1)
	else (acc_c, acc_d, s)) acc (Sparse.chl t.rn r)) 
      (IntSet.of_int t.r) ([], [], p.r) in
  (* c roots to d sites *)
  let (edg_c_rs1, edg_d_rs0, j0) = 
    IntSet.fold (fun r acc ->
      List.fold_left (fun (acc_c, acc_d, s) c ->
	((r, s) :: acc_c, (s, c) :: acc_d, s + 1)) acc (Sparse.chl t.rs r)) 
      (IntSet.of_int t.r) ([], [], s0) in
  (* c nodes to d nodes *)
  let (edg_c_ns0, edg_d_rn1, s1) = 
    IntSet.fold (fun r acc ->
      List.fold_left (fun (acc_c, acc_d, s) c ->
	if IntSet.mem c v_d then 
	  ((Iso.find iso_v_c r, s) :: acc_c, 
	   (s, Iso.find iso_v_d c) :: acc_d, 
	   s + 1)
	else (acc_c, acc_d, s)) acc (Sparse.chl t.nn r)) 
      v_c ([], [], j0) in
  (* c nodes to d sites *)
  let (edg_c_ns1, edg_d_rs1, j1) = 
    IntSet.fold (fun r acc ->
      List.fold_left (fun (acc_c, acc_d, s) c ->
	((Iso.find iso_v_c r, s) :: acc_c, 
	 (s, c) :: acc_d, s + 1)) acc (Sparse.chl t.ns r)) 
      v_c ([], [], s1)
  (* c roots to p nodes *)
  and edg_c_rp = 
    IntSet.fold (fun r acc ->
      List.fold_left (fun acc c ->
	if IntSet.mem c v_p' then 
	  let s = List.hd (Sparse.prn p.rn (Iso.find iso' c)) in 
	  (r, s) :: acc
	else acc) acc (Sparse.chl t.rn r)) 
      (IntSet.of_int t.r) []
  (* c nodes to p nodes *)
  and edg_c_np = 
    IntSet.fold (fun r acc ->
      List.fold_left (fun acc c ->
	if IntSet.mem c v_p' then 
	  let s = List.hd (Sparse.prn p.rn (Iso.find iso' c)) in 
	  (Iso.find iso_v_c r, s) :: acc
	else acc) acc (Sparse.chl t.nn r)) 
      v_c []
  (* p nodes to d nodes *)
  and edg_d_nn = 
    IntSet.fold (fun n acc ->
      List.fold_left (fun acc c ->
	if IntSet.mem c v_d then 
	  let s = List.hd (Sparse.chl p.ns (Iso.find iso' n)) in 
	  (s, Iso.find iso_v_d c) :: acc
	else acc) acc (Sparse.chl t.nn n)) 
      v_p' []
  (* p nodes to d sites *)
  and edg_d_ns = 
    IntSet.fold (fun n acc ->
      List.fold_left (fun acc c ->
	let s = List.hd (Sparse.chl p.ns (Iso.find iso' n)) in 
	(s, c) :: acc) acc (Sparse.chl t.ns n)) 
      v_p' [] in      
  let (c, d) =
    ((* Context c *)
      let n = IntSet.cardinal v_c 
      and s = p.r + j1 in
      { r = t.r;
	n = n;
	s = s;
	rn = Sparse.make t.r n;
	rs = Sparse.make t.r s;
	nn = Sparse.make n n;
	ns = Sparse.make n s;
      },
     (* Parameter d *)
     let n = IntSet.cardinal v_d
     and r = p.s + j1 in
     { r = r;
       n = n;
       s = t.s;
       rn = Sparse.make r n;
       rs = Sparse.make r s;
       nn = Sparse.make n n;
       ns = Sparse.make n s;
     }) in
  (* Add old edges *)
  Sparse.iter (fun i j ->
    if IntSet.mem j v_c then Sparse.add c.rn i (Iso.find iso_v_c j)) t.rn;
  Sparse.iter (fun  i j ->
  if (IntSet.mem i v_c) && (IntSet.mem j v_c) then
    Sparse.add c.nn (Iso.find iso_v_c i) (Iso.find iso_v_c j)
  else if (IntSet.mem i v_d) && (IntSet.mem j v_d) then
    Sparse.add d.nn (Iso.find iso_v_d i) (Iso.find iso_v_d j)) t.nn;
  Sparse.iter (fun i j ->
    if IntSet.mem i v_d then Sparse.add d.ns (Iso.find iso_v_d i) j) t.ns;
  (* Add new edges *)
  Sparse.add_list c.rs (edg_c_rs0 @  edg_c_rs1 @ edg_c_rp);
  Sparse.add_list c.ns (edg_c_ns0 @  edg_c_ns1 @ edg_c_np);
  Sparse.add_list d.rn (edg_d_rn0 @  edg_d_rn1 @ edg_d_nn);
  Sparse.add_list d.rs (edg_d_rs0 @  edg_d_rs1 @ edg_d_ns); 
  (c, d, elementary_id (j1 - p.r), iso_v_c, iso_v_d)

(*
exception PLACING of bg

(*  given a place graph p it returns a place graph p' a list of ions, the size
    of an identity and a placing. Return p if it does not contain any ions. The
    following invariant holds p = p' ((K x K x K x id) phi). An isomorphism to
    restore the original numbering of p is also computed: p' -> p  *)
let split_leaves p =
  if is_plc p then raise (PLACING p)
  else begin
    let ions = IntSet.of_list (Sparse.leaves p.nn) in
    (* p -> ions *)
    let ions_iso = IntSet.fix ions 
    and n_ions = IntSet.cardinal ions in
    let nodes_s = IntSet.diff (IntSet.of_list (Sparse.dom p.ns)) ions
    and roots_s = IntSet.of_list (Sparse.dom p.rs) in
    let ns_iso = IntSet.fix nodes_s 
    and rs_iso = IntSet.fix roots_s 
    and j = (IntSet.cardinal nodes_s) + (IntSet.cardinal roots_s) 
    and n_p' = IntSet.diff (IntSet.of_int p.n) ions in
    (* p -> p'*) 
    let p'_iso = IntSet.fix n_p' in
    let phi = 
      { r = n_ions + j;
	n = 0;
	s = p.s;
	rn = Sparse.make (n_ions + j) 0;
	rs = Sparse.make (n_ions + j) p.s; (* fill *)
	nn = Sparse.make 0 0;
	ns = Sparse.make 0 p.s;
      }	in
    (* edges from ions to sites *)
    IntSet.iter (fun i -> 
      let cs = Sparse.chl p.ns i in
      List.iter (fun j -> 
	Sparse.add phi.rs (Iso.find ions_iso i) j) cs) ions;
    (* edges from id_nodes to sites *)
    IntSet.iter (fun i -> 
      let cs = Sparse.chl p.ns i in
      List.iter (fun j -> 
	Sparse.add phi.rs ((Iso.find ns_iso i) + n_ions) j) cs) nodes_s;
    (* edges from id_roots to sites *)
    IntSet.iter (fun i -> 
      let cs = Sparse.chl p.rs i in
      List.iter (fun j -> 
	Sparse.add phi.rs 
	  ((Iso.find rs_iso i) + n_ions + (IntSet.cardinal nodes_s)) j)
	cs) roots_s;
    let p' =
      { r = p.r;
	n = p.n - n_ions;
	s = n_ions + j;
	rn = Sparse.make p.r (p.n - n_ions);
	rs = Sparse.make p.r (n_ions + j);
	nn = Sparse.make (p.n - n_ions) (p.n - n_ions);
	ns = Sparse.make (p.n - n_ions) (n_ions + j);
      } in
    (* edges from nodes to nodes/sites(ions) *)
    IntSet.iter (fun i ->
      let cs = Sparse.chl p.nn i in
      List.iter (fun j ->
	if IntSet.mem j ions then
	  Sparse.add p'.ns (Iso.find p'_iso i) (Iso.find ions_iso j)
	else Sparse.add p'.nn (Iso.find p'_iso i) (Iso.find p'_iso j))
	cs) n_p';
    (* edges from nodes to sites *)
    Iso.iter (fun i j ->
      Sparse.add p'.ns (Iso.find p'_iso i) (j + n_ions)) ns_iso;
    (* edges from roots to nodes/sites(ions) *)
    Sparse.iter (fun i j ->
      if IntSet.mem j ions then
	Sparse.add p'.rs r (Iso.find ions_iso j)
      else Sparse.add p'.rn r (Iso.find p'_iso j)) p.rn;
    (* edges from roots to sites *)
    Iso.iter (fun i j ->
      Sparse.add p'.rs r (j + n_ions + (IntSet.cardinal nodes_s))) rs_iso;
    (p', ions, j, phi, Iso.inverse p'_iso)
  end

(* recursively apply split leaves until a placing is returned. At each step the
   isos are converted to isos to p *)
(* INCOMPLETE *)
let levels p =
  let rec fix p acc iso =
    try
      let (p', ions, j, phi, iso') = split_leaves p in
      fix p' ((iso ions, j, phi) :: acc) (iso iso')
    with
    | PLACING phi -> (phi, acc) in
  let id_p = 
    Iso.of_list (List.combine (IntSet.of_int p.n) (IntSet.of_int p.n)) in
  fix p [] id_p  
*)

(* Compute three strings to build a dot representation.*)
let get_dot p =
  (* Root shapes *)
  let root_shapes =  
    IntSet.fold (fun i buff ->
      sprintf "%sr%d [ label=\"%d\", style=\"dashed\", shape=box, width=.28,\
                       height=.18, fontname=\"serif\", fontsize=9.0 ];\n" buff i i)
      (IntSet.of_int p.r) ""
  (* Site shapes *)
  and site_shapes = 
    IntSet.fold (fun i buff ->
      sprintf "%ss%d [ label=\"%d\", style=\"filled,dashed\", shape=box,\
                       fillcolor=\"gray\", width=.28, height=.18,\
                       fontname=\"serif\", fontsize=9.0 ];\n" buff i i)
      (IntSet.of_int p.s) ""
  (* Ranks *)
  and ranks = 
    List.fold_left (fun buff ns ->
      sprintf "%s{ rank=same; %s };\n" buff 
	(String.concat "; " (IntSet.fold (fun i acc ->
	  (sprintf "v%d" i) :: acc) ns []))) "" (Sparse.levels p.nn)
  (* Adjacency matrix *) 
  and m_rn = 
    Sparse.fold (fun i j buff ->
      sprintf "%sr%d -> v%d [ arrowhead=\"vee\", arrowsize=0.5 ];\n" 
	buff i j) p.rn ""
  and m_rs =
    Sparse.fold (fun i j buff ->
      sprintf "%sr%d -> s%d [ arrowhead=\"vee\", arrowsize=0.5 ];\n" 
	buff i j) p.rs ""
  and m_nn =
    Sparse.fold (fun i j buff ->
      sprintf "%sv%d -> v%d [ arrowhead=\"vee\", arrowsize=0.5 ];\n" 
	buff i j) p.nn ""
  and m_ns =
    Sparse.fold (fun i j buff ->
      sprintf "%sv%d -> s%d [ arrowhead=\"vee\", arrowsize=0.5 ];\n" 
	buff i j) p.ns "" in
  (root_shapes, site_shapes, ranks, String.concat "" [m_rn; m_rs; m_nn; m_ns])

(* Number of edges in the DAG *)
let edges p =
  (Sparse.entries p.rn) + (Sparse.entries p.rs) + 
    (Sparse.entries p.nn) + (Sparse.entries p.ns)

(* given an edge of control A -> B, find all the edges with the same type.
   return a hash table (string * string) -> (int * int) *)
let partition_edges p n =
  let h = Hashtbl.create (Sparse.entries p.nn) in
  Sparse.iter (fun i j ->
    let (a, b) = (Nodes.find n i, Nodes.find n j) in
    match (a, b) with 
    | (Ctrl.Ctrl(a_string, _), Ctrl.Ctrl(b_string, _)) ->
      Hashtbl.add h (a_string, b_string) (i, j)) p.nn;
  h

type deg =
| V of int (* only vertices *)
| S of int (* with sites or roots *)

let indeg p i =
  assert (i >= 0);
  let d = List.length (Sparse.prn p.nn i) in
  match Sparse.prn p.rn i with
  | [] -> V d
  | _ -> S d

let outdeg p i =
  assert (i >= 0);
  let d = List.length (Sparse.chl p.nn i) in
  match Sparse.chl p.ns i with
  | [] -> V d
  | _ -> S d

(* true if the degrees are compatible *)
let compat_deg t p =
  match p with
  | V d -> begin match t with
    | V d' -> d = d' 
    | S _ -> false
    end
  | S d -> begin match t with
    | V d' -> d' >= d
    | S d' -> d' >= d
  end

let compat t p t_i p_i =
  (compat_deg (indeg t t_i) (indeg p p_i)) && 
    (compat_deg (outdeg t t_i) (outdeg p p_i))

(* Input:  number of nodes in the target and list of incompatible nodes in the
           pattern. 
   Output: [!(i, j) and  ...] *)
let block_rows t_n : int list -> Cnf.clause list =
  let j_t = IntSet.of_int t_n in
  List.fold_left (fun acc i ->
    IntSet.fold (fun j acc ->
      [Cnf.N_var (Cnf.M_lit (i, j))] :: acc) j_t acc) []
    
(* Optimisation 1:
   - Do not add a pair if one of the two literals matches nodes of different 
     controls.
   - Check degrees
   Optimisation 2 - match edges instead of enumerate non matching pairs: 
   - An edge in the pattern is matched exactly to one edge in the target. 
     Algorithm:
      0. (optional) Create an hashmap (ctrl,ctrl) -> (i,j) to partition t edges 
      1. Iterate over the edges in p and for every (i, j) enforce exactly one
         match with one of the edges in t of equal control.
      2. Avoid "at-least" and "at-most" constraints ()*)
(*Tseitin*)
(*if ij then ab or cd or ... de ENCODING = (ic and jd) or ... (id and je)*)
let match_list t p n_t n_p : 
    (Cnf.clause * Cnf.b_clause list) list * Cnf.clause list  * Cnf.clause list =
  let h = partition_edges t n_t in
  let (clauses, b, clauses_exc) = Sparse.fold (fun i j (acc, block, exc) ->
    let (a, b) = (Nodes.find n_p i, Nodes.find n_p j) in
    match (a, b) with 
    | (Ctrl.Ctrl(a_string, _), Ctrl.Ctrl(b_string, _)) -> begin
      let t_edges = Hashtbl.find_all h (a_string, b_string) in
      try
	let (clause, pairs) = 
	  Cnf.tseitin (List.fold_left (fun acc (i', j') ->
	    (* Degree check *)
	    if (compat t p i' i) && (compat t p j' j) then
	      (Cnf.M_lit (i, i'), Cnf.M_lit (j, j')) :: acc
	    else acc) [] t_edges) in
	if List.length clause = 0 then begin
	  (* No compatible edges found *)
	  (acc, i :: j :: block, exc) 
	end else 
	  ((clause, pairs) :: acc, block, exc)
      with
      | Cnf.TSEITIN clauses ->
	match clauses with
	| [] -> (acc, block, exc)
	| _ -> (acc, block, clauses @ exc)
    end) p.nn ([], [], []) in
  (clauses, block_rows t.n b, clauses_exc)

(* EQUALITY functions *)
(* out clauses = (ij1 or ij2 or ij ...) :: ... *)
let match_root_nodes a b n_a n_b : Cnf.clause list =
  Sparse.fold (fun r i acc ->
    let c = Nodes.find n_a i in 
    let children = 
      List.filter (fun i -> 
	Ctrl.(=) c (Nodes.find n_b i)) (Sparse.chl b.rn r) in
    (List.map (fun j -> 
      Cnf.P_var (Cnf.M_lit (i, j))) children) :: acc) a.rn []

(*Dual*)
let match_nodes_sites a b n_a n_b : Cnf.clause list =
  Sparse.fold (fun i s acc ->
    let c = Nodes.find n_a i in 
    let parents = 
      List.filter (fun i -> 
	Ctrl.(=) c (Nodes.find n_b i)) (Sparse.prn b.ns s) in
    (List.map (fun j -> 
      Cnf.P_var (Cnf.M_lit (i, j))) parents) :: acc) a.ns []
    
(* Not needed. Just Sparse.compare a.rs b.rs *)
(* let compare_roots_sites a b = *)
(*   let (_, a_s, _, _) = split a.m a.r a.n *)
(*   and (_, b_s, _, _) = split b.m b.r b.n in *)
(*   compare a_s b_s *)

(* Not needed. Just Sparse.(=) a.rs b.rs *)
(* let match_roots_sites a b =  *)
(*   (compare_roots_sites a b) = 0 *)
  
(* Not needed. Just Sparse.dom b.ns *)
(* Returns the set of nodes (columns) having at least one site child *)
(* let nodes_site_child b =  *)
(*   IntSet.filter (fun j -> *)
(*     not (IntSet.is_empty (IntSet.filter (fun j -> *)
(* 	     j >= b.n) (chl b.m (j + b.r))))) (of_int b.n) *)

(* Dual *)
(* Not needed. Just Sparse.codom b.rn *)
(* let nodes_root_par b =  *)
(*   IntSet.filter (fun j -> *)
(*     not (IntSet.is_empty (IntSet.filter (fun i -> *)
(* 	     i < b.n) (prn b.m j)))) (of_int b.n) *)

(* Nodes with no children (both nodes and sites). *)
let leaves p =
  let l_n = Sparse.leaves p.nn
  and l_s = Sparse.leaves p.ns in
  IntSet.inter l_n l_s

(* Dual *)
let orphans p =
  let o_r = Sparse.orphans p.rn
  and o_n = Sparse.orphans p.nn in
  IntSet.inter o_r o_n

(* leves (orphans) in p are matched to leaves (orphans) in t.
   C5: ij0 or ij1 or ..... *)
let match_leaves t p n_t n_p : Cnf.clause list * Cnf.clause list =
  let l_p = leaves p
  and l_t = leaves t in
  let (clauses, b) =
    IntSet.fold (fun i (acc, block) ->
      let c = Nodes.find n_p i in
      let clause = IntSet.fold (fun j acc ->
	Cnf.P_var (Cnf.M_lit (i, j)) :: acc) (IntSet.filter (fun j -> 
	  Ctrl.(=) c (Nodes.find n_t j)) l_t) [] in
      (* If clause is empty i is not matchable *)
      match clause with
      | [] -> (acc, i :: block)
      | _ -> (clause :: acc, block)) l_p ([], []) in
  (clauses, block_rows t.n b)

(* Dual *)
let match_orphans t p n_t n_p : Cnf.clause list * Cnf.clause list =
  let o_p = orphans p 
  and o_t = orphans t in
  let (clauses, b) =
    IntSet.fold (fun i (acc, block) ->
      let c = Nodes.find n_p i in
      let clause = IntSet.fold (fun j acc ->
	Cnf.P_var (Cnf.M_lit (i, j)) :: acc) (IntSet.filter (fun j -> 
	  Ctrl.(=) c (Nodes.find n_t j)) o_t) [] in
      match clause with
      | [] -> (acc, i :: block)
      | _ -> (clause :: acc, block)) o_p ([], []) in
  (clauses, block_rows t.n b)
  
(* Only ctrl and deg check *)
let match_sites t p n_t n_p : Cnf.clause list * Cnf.clause list =
  let (clauses, b) =
    Sparse.fold (fun i _ (acc, block) -> 
      let c = 
	match Nodes.find n_p i with 
	| Ctrl.Ctrl (c, _) -> c in
      let js = List.filter (fun j ->
	(compat_deg (indeg t j) (indeg p i))) (Nodes.find_all n_t c) in
      match js with
      | [] -> (acc, i :: block)
      | _ -> begin
	let clause = List.map (fun j ->
	  Cnf.P_var (Cnf.M_lit (i, j))) js in
	(clause :: acc, block)
      end) p.ns ([], []) in
  (clauses, block_rows t.n b)
    
let match_roots t p n_t n_p : Cnf.clause list * Cnf.clause list =
  let (clauses, b) =
    Sparse.fold (fun _ i (acc, block) -> 
      let c = 
	match Nodes.find n_p i with 
	| Ctrl.Ctrl (c, _) -> c in
      let js = List.filter (fun j ->
	(compat_deg (outdeg t j) (outdeg p i))) (Nodes.find_all n_t c) in
      match js with
      | [] -> (acc, i :: block)
      | _ -> begin
	let clause = List.map (fun j ->
	  Cnf.P_var (Cnf.M_lit (i, j))) js in
	(clause :: acc, block)
      end) p.rn ([], []) in
  (clauses, block_rows t.n b)

(* Block unconnected pairs of nodes with sites and nodes with roots. *)
let match_trans t p : Cnf.clause list =
  let n_s = Sparse.dom p.ns (* index i *)
  and n_r = Sparse.codom p.rn in (* index j *)
  let (n_s', n_r') = 
    IntSet.fold (fun i (acc_s, acc_r) ->
      let x = IntSet.inter (IntSet.of_list (Sparse.chl p.nn i)) n_r in
      if IntSet.cardinal x = 0 then (acc_s, acc_r)
      else (IntSet.remove i acc_s, IntSet.diff acc_r x)
    ) n_s (n_s, n_r) in
  (* For every edge (i', j') in t, block any ii' and jj' *)
  Sparse.fold (fun i' j' acc ->
    let blocks = IntSet.fold (fun i acc ->
      IntSet.fold (fun j acc ->
	[Cnf.N_var (Cnf.M_lit (i, i')); Cnf.N_var (Cnf.M_lit (j, j'))] :: acc
      ) n_r' acc
    ) n_s' [] in
    blocks @ acc) t.nn [] 

let check_sites t p v_p' c_set iso =
  let s_set =
    IntSet.fold (fun j acc ->
      let children = IntSet.of_list (Sparse.chl t.ns j) in
      IntSet.union acc children) v_p' IntSet.empty in
  (* Is there a set of sites with the same parent set? *)
  (* Nodes *)
  (IntSet.for_all (fun c ->
    let prn_c = IntSet.inter 
      v_p' (IntSet.of_list (Sparse.prn t.nn c)) in
    (* Construct a candidate set of sites *)
    let candidate =
      IntSet.fold (fun s acc ->
	let prn_s = IntSet.apply 
	  (IntSet.of_list (Sparse.prn p.ns s)) iso in
	if IntSet.subset prn_s prn_c then IntSet.union prn_s acc
	else acc
      ) (IntSet.of_int p.s) IntSet.empty in
    (* Equality test *)
    IntSet.equal candidate prn_c) c_set) &&
    (* Sites *)
    (IntSet.for_all (fun s ->
      let prn_s = IntSet.inter 
	v_p' (IntSet.of_list (Sparse.prn t.ns s)) in
    (* Construct a candidate set of sites *)
    let candidate =
      IntSet.fold (fun s acc ->
	let prn_s' = IntSet.apply 
	  (IntSet.of_list (Sparse.prn p.ns s)) iso in
	if IntSet.subset prn_s' prn_s then IntSet.union prn_s' acc
	else acc
      ) (IntSet.of_int p.s) IntSet.empty in
    (* Equality test *)
    IntSet.equal candidate prn_s) s_set)

(* Dual *)
let check_roots t p v_p' iso =
  let p_set = 
    IntSet.fold (fun j acc ->
      let parents = IntSet.diff 
	(IntSet.of_list (Sparse.prn t.nn j)) v_p' in
      IntSet.union acc parents) v_p' IntSet.empty
  and r_set = 
    IntSet.fold (fun j acc ->
      let parents = IntSet.of_list (Sparse.prn t.rn j) in
      IntSet.union acc parents) v_p' IntSet.empty in
  (* Is there a set of roots with the same children set? *)
  (* Nodes *)
  (IntSet.for_all (fun x ->
    let chl_p = IntSet.inter 
      v_p' (IntSet.of_list (Sparse.chl t.nn x)) in
    (* Construct a candidate set of roots *)
    let candidate =
      IntSet.fold (fun r acc ->
	let chl_r = IntSet.apply 
	  (IntSet.of_list (Sparse.chl p.rn r)) iso in
	if IntSet.subset chl_r chl_p then IntSet.union chl_r acc
	else acc
      ) (IntSet.of_int p.r) IntSet.empty in
    (* Equality test *)
    IntSet.equal candidate chl_p) p_set) &&
    (* Roots *)
    (IntSet.for_all (fun x ->
      let chl_r = IntSet.inter 
	v_p' (IntSet.of_list (Sparse.chl t.rn x)) in
    (* Construct a candidate set of roots *)
      let candidate =
	IntSet.fold (fun r acc ->
	let chl_r' = IntSet.apply 
	  (IntSet.of_list (Sparse.chl p.rn r)) iso in
	if IntSet.subset chl_r' chl_r then IntSet.union chl_r' acc
	else acc
	) (IntSet.of_int p.r) IntSet.empty in
      (* Equality test *)
      IntSet.equal candidate chl_r) r_set)
    
(* check TRANS *)
let check_trans t_trans v_p' c_set = 
  (* check if there is a node child of co-domain, outside co-domain, such that
     one of its children in trans is in co-domain *)
  not (IntSet.exists (fun c ->
    List.exists (fun t ->
      IntSet.mem t v_p') (Sparse.chl t_trans c)) c_set)
    
(* Check if iso i : p -> t is valid *)
let check_match t p t_trans iso =  
  let v_p' = IntSet.of_list (Iso.codom iso) in
  let c_set =
    IntSet.fold (fun j acc ->
      let children = IntSet.diff 
	(IntSet.of_list (Sparse.chl t.nn j)) v_p' in
      IntSet.union acc children) v_p' IntSet.empty in
  (check_sites t p v_p' c_set iso) && (check_roots t p v_p' iso) && 
    (check_trans t_trans v_p' c_set)
    
