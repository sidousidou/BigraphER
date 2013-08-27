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
    rn = Sparse.make 0 0;
    rs = Sparse.diag n; 
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 0;
  }
    
let elementary_merge n =
  assert (n >= 0);
  { r = 1;
    n = 0;
    s = n;
    rn = Sparse.make 0 0;
    rs = Sparse.row_1 n; 
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 0;
  }
    
let elementary_split n =
  assert (n >= 0);
  { r = n;
    n = 0;
    s = 1;
    rn = Sparse.make 0 0;
    rs = Sparse.col_1 n; 
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 0;
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
    rn = Sparse.make 0 0;
    rs = Sparse.stack 
      (Sparse.append (Sparse.make n m) (Sparse.diag n)) 
      (Sparse.append (Sparse.diag m) (Sparse.make m n)); 
    nn = Sparse.make 0 0;
    ns = Sparse.make 0 0;
  }
    
let elementary_ion =
  { r = 1;
    n = 1;
    s = 1;
    rn = Sparse.row_1 1;
    rs = Sparse.make 0 0;
    nn = Sparse.make 0 0;
    ns = Sparse.row_1 1;
  }

(* Parse a placing *)
let parse_placing l r =
  assert (r >= 0);
  { r = r;
    n = 0; 
    s = List.length l;
    rn = Sparse.make 0 0;
    rs = Sparse.parse_vector l r;
    nn = Sparse.make 0 0;      
    ns = Sparse.make 0 0;
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

(* Tensor product: A x B (indices in the right handside are increased) *)
let tens a b =
  { r = a.r + b.r;
    n = a.n + b.n;
    s = a.s + b.s;
    rn = Sparse.tens a.rn b.rn;
    rs = Sparse.tens a.rs b.rs;
    nn = Sparse.tens a.nn b.nn;
    ns = Sparse.tens a.ns b.ns;
  }
    
(* Composition: G o F (indices in the right handside are increased) *)
let comp g f =
  if g.s = f.r then { r = g.r;
		      n = g.n + f.n;
		      s = f.s;
		      rn = Sparse.append g.rn (Sparse.mul g.rs f.rn);
		      rs = Sparse.mul g.rs f.rs;
		      nn = Sparse.tens g.nn (Sparse.mul g.ns f.rn);
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
  match Sparse.orphans slice with
  | [] -> Sparse.siblings_chk slice
  | _ -> false
    
(* Is p epimorphic: no root is idle and no two roots are partners *)
let is_epi p =
  let slice = Sparse.append p.rn p.rs in
  match Sparse.leaves slice with
  | [] -> Sparse.partners_chk slice
  | _ -> false
    
(* Is p guarded: no root has sites as children *)
let is_guard p = Sparse.is_0 p.rs
 
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
      { r = t.r;
	n = IntSet.cardinal v_c;
	s = p.r + j1;
	rn = Sparse.make t.r n;
	rs = Sparse.make t.r (p.r + j1);
	nn = Sparse.make n n;
	ns = Sparse.make n (p.r + j1);
      },
     (* Parameter d *)
     { r = p.s + j1;
       n = IntSet.cardinal v_d;
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
  (c, d, elementary_id (j1 - p.r),  iso_v_c, iso_v_d)

(* Parallel composition of n ions *)
let elementary_ions n =
  let rec fold i acc =
    if i < 0 then acc
    else fold (i - 1) (tens elementary_ion acc) in
  fold (n - 1) id0

(* Construct the placing for a level. Firts two arguments are columns while
   last two are rows. *)
let build_phi sites sites_id roots roots_id p =
  (* Generate an map with an offset in the codomain *) 
  let fix_off target offset =
    Array.to_list (Array.mapi (fun index i ->
      (i, index + offset)) (Array.of_list target)) in
  (* Iso from inputs to phi indices *) 
  let iso_roots = fix_num roots
  and map_roots_id = fix_off roots_id (IntSet.cardinal roots) in
  (* Apply isos to parent sets *)
  let map_par (s : IntSet.t) = 
    fst (IntSet.fold (fun j (acc, map) ->
      (* parents in roots and in roots_id *) 
      let p_r, p_s =
	IntSet.partition (fun i -> IntSet.mem i roots) (prn p.m j) in
      (* map p_s to p_s0 and update map_roots_id *)
      let p_s0, new_map =
        IntSet.fold (fun i (acc, m) ->
          ((try (List.assoc i m) :: acc with _ -> acc),
           List.remove_assoc i m)) p_s ([], map) in
      (acc @ [(IntSet.elements (apply p_r iso_roots)) @ p_s0], new_map)
    ) s ([], map_roots_id)) in
  (* parents of sites *)  
  let (prn_sites : int list list) = 
       map_par sites  
  (* parents of sites_id *)  
  and (prn_id : int list list) = 
       map_par sites_id in   
  parse_placing (prn_sites @ prn_id)
    ((IntSet.cardinal roots) + (List.length roots_id))  

(* Compute the levels of p. Indeces are columns. *)
let levels p =
  (* leaves = columns already in a level*) 
  let rec loop p sites sites_id res leaves leaves_init =
    let roots, roots_id, prop =
      IntSet.fold (fun j (acc, l, s) ->
        (* Partition the set of parents of a site *)
        let keep, discard =
          IntSet.partition (fun i ->
            IntSet.subset (chl p.m i) leaves) (prn p.m j) in
	(* Keep only new roots *)    
        let new_keep = IntSet.filter (fun i ->
          not (IntSet.mem (i - p.r) leaves)) keep in
	 if IntSet.is_empty discard then
          (IntSet.union new_keep acc, (IntSet.elements discard) @ l, s)
        else
          (IntSet.union new_keep acc, (IntSet.elements discard) @ l,
           IntSet.add j s)
      ) (IntSet.union sites sites_id) (leaves_init, [], IntSet.empty) in
    (* If no nodes in parents *)         
    if IntSet.is_empty
      (IntSet.filter (fun i ->
	i >= p.r) (IntSet.union roots (set_of_list roots_id)))
    then
      (* Build topmost placing and return levels *)
      (build_phi sites sites_id (of_int p.r) [] p, res)
    else
      (* build current level and iterate loop *)
      (* placing *)
      let phi = build_phi sites sites_id roots roots_id p
      (* nodes (columns) not in leaves *)
      and ions = IntSet.fold (fun i acc ->
          if (i >= p.r) && not (IntSet.mem (i - p.r) leaves) then
            IntSet.add (i - p.r) acc
          else acc) roots IntSet.empty in  
      loop p ions prop ((ions, IntSet.cardinal prop, phi) :: res)
	(IntSet.union ions leaves) IntSet.empty in    
  let sites = off p.n (of_int p.s) 
  and leaves = zero_rows p.m in
  loop p sites IntSet.empty [] sites leaves	 

(* Compute three strings to build a dot representation.*)
(* USE CSS *)
let get_dot p =
  (
    (* Root shapes *)
    IntSet.fold (fun i buff ->
      buff ^ (sprintf "r%d [label=\"%d\", style=dashed, shape=box, width=.28, height=.18];\n" i i)
    ) (of_int p.r) "",
  (* Site shapes *)
    IntSet.fold (fun i buff ->
      buff ^ (sprintf
		"s%d [label=\"%d\", style=\"filled,dashed\", shape=box, fillcolor=gray, width=.28, height=.18];\n"
		i i)) (of_int p.s) "",
  (* ranks *)
    String.concat "" (List.map (fun (s, _, _) ->
      sprintf "{rank=same; %s};\n"
	(String.concat "; " (List.map (fun i ->
	  sprintf "v%d" i) (IntSet.elements s)))) (snd (levels p))),  
  (* Adjacency matrix *) 
  let out = ref "" in
  for i = 0 to p.r + p.n - 1 do
    for j = 0 to p.n + p.s - 1 do
      if p.m.{i,j} = 1 then
        let new_i = 
          if i < p.r then
            (* Root *)
            sprintf "r%d" i
          else
            (* Node *)
            sprintf "v%d" (i - p.r)
        and new_j = 
          if j < p.n then
            (* Node *)
            sprintf "v%d" j
          else
            (* Site *)
            sprintf "s%d" (j - p.n) in             
        out := !out ^ (sprintf "%s -> %s [arrowhead=\"vee\" arrowsize=0.5];\n" new_i new_j)
      else ()
    done
  done;
  !out)

(* Returns a string representation of a placing. *)
let snf_of_placing p =
  let out = ref [] in
  for j = p.s - 1 downto 0 do
    let parents = ref [] in
    for i = p.r - 1 downto 0 do
      if p.m.{i,j} = 1 then
        parents := (sprintf "%d" i) :: !parents
      else ()
    done;
    out := (sprintf "{%s}" (String.concat "," !parents)) :: !out
  done;
  sprintf "([%s],%d)" (String.concat "," !out) p.r  

(* Counts the number of edges in the DAG *)
let edges p =
  let e = ref 0 in
  for i = 0 to p.r + p.n - 1 do
    for j = 0 to p.n + p.s - 1 do
      e := p.m.{i,j} + !e
    done
  done;
  !e

(* GPROF *)
(* Returns a list of pairs of non-iso nodes. Every node is expressed as a 
   pair of indices. *) (* USE ISO instead*)
let match_list t p =
  let res = ref [] in
  for i = p.r to p.r + p.n - 1 do
    for l = 0 to p.n - 1 do
      for j = t.r to t.r + t.n - 1 do
        for k = 0 to t.n - 1 do
          if p.m.{i,l} <> t.m.{j,k} then
            res := (i - p.r, l, j - t.r, k) :: !res
          else ()
        done
      done
    done
  done;
  !res          

let match_root_nodes a b =
  let res = ref [] in
  for i = 0 to a.r - 1 do
    for l = 0 to a.n - 1 do
      for k = 0 to b.n - 1 do
        if a.m.{i,l} <> b.m.{i,k} then
          res := (l, k) :: !res
        else ()
      done
    done
  done;
  !res          

let match_nodes_sites a b =
  let res = ref [] in
  for i = a.r to a.r + a.n - 1 do
    for j = b.r to b.r + b.n - 1 do
      for k = a.n to a.n + a.s - 1 do
        if a.m.{i,k} <> b.m.{j,k} then
          res := (i - a.r, j - b.r) :: !res
        else ()
      done
    done
  done;
  !res          

let compare_roots_sites a b =
  let (_, a_s, _, _) = split a.m a.r a.n
  and (_, b_s, _, _) = split b.m b.r b.n in
  compare a_s b_s

let match_roots_sites a b = 
  (compare_roots_sites a b) = 0
  
(* Returns the set of nodes (columns) having at least one site child *)
let nodes_site_child b = 
  IntSet.filter (fun j ->
    not (IntSet.is_empty (IntSet.filter (fun j ->
	     j >= b.n) (chl b.m (j + b.r))))) (of_int b.n)

(* Dual *)
let nodes_root_par b = 
  IntSet.filter (fun j ->
    not (IntSet.is_empty (IntSet.filter (fun i ->
	     i < b.n) (prn b.m j)))) (of_int b.n)

let match_leaves t p =
  (* forbid pairs (0,n>0) children *)
  let leaves_p =  IntSet.filter (fun x -> x >= 0) (off (-p.r) (zero_rows p.m)) 
  and non_leaves_t = IntSet.diff (of_int t.n) (IntSet.filter (fun x ->
    x >= 0) (off (-t.r) (zero_rows t.m))) in
  IntSet.fold (fun i acc ->
    Iso.union acc (IntSet.fold (fun j acc ->
      Iso.add (i,j) acc) non_leaves_t Iso.empty)) leaves_p Iso.empty

let match_orphans t p =
  (*forbid pairs (0,n>0) roots *)
  let orphans_p = IntSet.filter (fun x -> x < p.n) (zero_cols p.m) 
  and non_orphans_t = IntSet.diff (of_int t.n) (IntSet.filter (fun x ->
    x < t.n) (zero_cols t.m)) in
  IntSet.fold (fun i acc ->
    Iso.union acc (IntSet.fold (fun j acc ->
      Iso.add (i,j) acc) non_orphans_t Iso.empty)) orphans_p Iso.empty 

(* Add easy blocking pairs, Iso checking of the siblings and partners is left out *)
let match_sites t p =
  let n_p =  nodes_site_child p 
  and n_t = of_int t.n in
  IntSet.fold (fun i acc ->
    Iso.union acc (IntSet.fold (fun j acc ->
      if (IntSet.cardinal (partners t j)) < (IntSet.cardinal (partners p i)) then
	((*printf "match_sites (%d,%d)\n" i j;*)
	Iso.add (i,j) acc)
      else
	acc) n_t Iso.empty)) n_p Iso.empty

let match_roots t p =
  let n_p =  nodes_root_par p 
  and n_t = of_int t.n in
  IntSet.fold (fun i acc ->
    Iso.union acc (IntSet.fold (fun j acc ->
      if (IntSet.cardinal (siblings t j)) < (IntSet.cardinal (siblings p i)) then
	((*printf "match_sites (%d,%d)\n" i j;*)
	Iso.add (i,j) acc)
      else
	acc) n_t Iso.empty)) n_p Iso.empty

(* check if iso i : p -> t is valid *)
let is_match_valid t p t_trans iso = 
  (* check SITES *)
  let check_sites t p iso =
    let n_t_sites = 
      IntSet.fold (fun j acc ->
	IntSet.union acc (chl t.m j)) (* diff sites t ??? NO *) 
	(off t.r (codom iso)) IntSet.empty in
    IntSet.for_all (fun c -> 
      (* is there a set of sites with the same parent set? *)
      let prn_c = 
	IntSet.inter (codom iso) (off (-t.r) (prn t.m c)) in
      (* construct a candidate set of sites *)
      let candidate = 
	IntSet.fold (fun s acc ->
	  let prn_s =
	    apply (IntSet.filter (fun x ->
	      x >= 0) (off (-p.r) (prn p.m s))) iso in
	  if IntSet.subset prn_s prn_c then
	    IntSet.union prn_s acc
	  else
	    acc) (off (p.n) (of_int p.s)) IntSet.empty in
      IntSet.equal candidate prn_c)
      (IntSet.diff n_t_sites (codom iso)) (* diff codom iso ??? YES *)
  (* check ROOTS (dual) *)
  and check_roots t p iso =
    let n_t_roots = (* rows *)
      IntSet.fold (fun j acc ->
	IntSet.union acc (prn t.m j)) (* diff roots t ??? NO *) 
	(codom iso) IntSet.empty in
    IntSet.for_all (fun _p -> 
      (* is there a set of roots with the same child set? *)
      let chl_p = (* cols *)
	IntSet.inter (codom iso) (chl t.m _p) in
      (* construct a candidate set of roots *)
      let candidate = (* cols *)
	IntSet.fold (fun r acc ->
	  let chl_r = 
	    apply (IntSet.filter (fun x ->  x < p.n) (chl p.m r)) iso in
	  if IntSet.subset chl_r chl_p then
	    IntSet.union chl_r acc
	  else
	    acc) (of_int p.r) IntSet.empty in
      IntSet.equal candidate chl_p)
      (IntSet.diff n_t_roots (off t.r (codom iso))) (* diff codom iso ??? YES *)
  (* check TRANS *)
  and check_trans t t_trans iso =
   (* check if there is a node child of codomain, outside codomain, such that
      one of its children in trans is in codomain *)
    not (IntSet.exists (fun c ->
      IntSet.exists (fun t -> 
	IntSet.mem t (codom iso)) (chl t_trans c))
	   (IntSet.diff (IntSet.fold (fun x acc ->
	     IntSet.union acc (IntSet.filter (fun j -> 
	       j < t.n) (chl t.m x))) (off t.r (codom iso)) IntSet.empty)
	      (codom iso))) in
  (check_sites t p iso) && (check_roots t p iso) && (check_trans t t_trans iso)

(*let match_trans t p t_trans blocks =
  let n_t = of_int t.n 
  (* pairs in t_trans *)
  and pairs_t = to_iso t_trans in
  (* pairs s_p x n_t *)
  let pairs_s_t = set_cart (nodes_site_child p) n_t
  (* pairs r_p x n_t *)
  and pairs_r_t = set_cart (nodes_root_par p) n_t in
  
  (* blocking pairs are (s,i(s)), (r, i(r)) if (i(s), i(r)) in t_trans *)
  
*)

(* DEBUG *)
(*let _ = 
  (* Example from page 82 *)
  let p = {r = 1; n = 3; s = 2; m = make 4 5} 
  and t = {r = 1; n = 8; s = 2; m = make 9 10} in
  p.m.Matrix.m.(0).(0) <- true;
  p.m.Matrix.m.(1).(1) <- true;
  p.m.Matrix.m.(1).(3) <- true;
  p.m.Matrix.m.(2).(2) <- true;
  p.m.Matrix.m.(3).(4) <- true;
  t.m.Matrix.m.(0).(0) <- true;
  t.m.Matrix.m.(1).(1) <- true;
  t.m.Matrix.m.(1).(2) <- true;
[6~  t.m.Matrix.m.(1).(3) <- true;
  t.m.Matrix.m.(2).(4) <- true;
  t.m.Matrix.m.(3).(5) <- true;
  t.m.Matrix.m.(4).(6) <- true;
  t.m.Matrix.m.(5).(7) <- true;
  t.m.Matrix.m.(6).(7) <- true;
  t.m.Matrix.m.(7).(9) <- true;
  t.m.Matrix.m.(8).(8) <- true;
  printf "pattern:\n%starget:\n%s" (string_of_pg p) (string_of_pg t);
  List.iter (fun (a, b, c, d) ->
    printf "(%d,%d,%d,%d) " a b c d)(match_list t p);
  printf "\n";
  let psi = parse_placing [[0;2]; [1;2]; []; [1]] 3 in
  printf "placing: %s\n" (snf_of_placing psi)
*) 

	
