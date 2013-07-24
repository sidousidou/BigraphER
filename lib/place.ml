open Printf
open Base
open Matrix

(* Type for concrete place graphs. The elements are roots, nodes, sites and
   adjacency matrix *)
type pg = {r: int; n: int; s: int; m: bmatrix}

(* Raised by comp. The elements are (sites, roots) *)
exception COMP_ERROR of (int * int) 

(* Not exposed *)
(*exception EPI of bool
exception MONO of bool*)

(* String representation *)
let string_of_pg p =
  sprintf "%d %d %d\n%s\n" p.r p.n p.s (to_string p.m)

(* Representation for match *)
let match_string p = (p.r, p.s, to_string p.m)

(* Apply isomorphism *)  
let apply_iso i p =
  { r = p.r;
    n = p.n;
    s = p.s;
    m = Matrix.apply_iso i p.m p.r
  }

(* Elementary place graphs *)
let elementary_id n =
  { r = n;
    n = 0;
    s = n;
    m = diag n
  }
    
let elementary_merge n =
  { r = 1;
    n = 0;
    s = n;
    m = row_1 n
  }
    
let elementary_split n =
  { r = n;
    n = 0;
    s = 1;
    m = col_1 n
  }

let id0 = elementary_id 0

let one = elementary_merge 0

let zero = elementary_split 0

let elementary_sym m n =
  { r = m + n;
    n = 0;
    s = m + n;
    m = stack (append (make_0 n m) (diag n)) (append (diag m) (make_0 m n))
  }
    
let elementary_ion =
  { r = 1;
    n = 1;
    s = 1;
    m = diag 2
  }

(* Parse a placing *)
let parse_placing l r =
  let v = parse_vector l r in
  {r = r;
   n = 0; 
   s = List.length l;
   m = v}

(* placing equality *)
let equal_placing a b =
  (a.r = b.r) && (a.s = b.s) && (a.m = b.m)

(* placing compare *)  
let compare_placing a b =
  let x = a.r - b.r in
  match x with
    | 0 -> (let x = a.s - b.s in
	    match x with
	      | 0 -> compare a.m b.m
	      | _ -> x)
    | _ -> x
    
(* Tensor product: A x B (indices in the right handside are increased) *)
let tens a b =
  let (m_s, m_n, m_v, m_r) = split a.m a.r a.n
  and (n_s, n_n, n_v, n_r) = split b.m b.r b.n in
  let res = 
    stack
      (append m_s (append (make_0 a.r b.n) (append m_n (make_0 a.r b.s))))
      (stack
	 (append (make_0 b.r a.n) (append n_s (append (make_0 b.r a.s) n_n)))
	 (stack
            (append m_v (append (make_0 a.n b.n) (append m_r (make_0 a.n b.s))))
            (append (make_0 b.n a.n) (append n_v 
					(append (make_0 b.n a.s) n_r))))) in
  { r = a.r + b.r;
    n = a.n + b.n;
    s = a.s + b.s;
    m = res
  }
    
(* Composition: G o F (indices in the right handside are increased) *)
let comp g f =
  (*printf "COMP\na:\n%s\nb:\n%s\n" (string_of_pg g) (string_of_pg f);*)
  if g.s = f.r then
    let (a, b, _, _) = split g.m (g.r + g.n) g.n
    and (c, _, d, _) = split f.m f.r (f.n + f.s) in
    let res =
      stack (append a (mul b c)) (append (make_0 f.n g.n) d) in
    (*printf "res:\n%s\n" (to_string res);*)
    { r = g.r;
      n = g.n + f.n;
      s = f.s;
      m = res
    }
  else raise (COMP_ERROR (g.s, f.r))

(* Is p an identity? *)
let is_id p =
  match p with
  | {r = x; n = 0; _} -> p.m = diag x
  | _ -> false

let is_plc p = p.n = 0
  
(* Is p monomorphic?: no two sites are siblings and no site is an orphan *)
let is_mono p =
  (* Orphan sites? *)  
  not (Int_set.exists (fun j ->
    prn p.m j = Int_set.empty) (off p.n (of_int p.s)))
  &&
    (* Roots or nodes with more than one site? *)
    not (Int_set.exists (fun i ->
      (Int_set.cardinal (Int_set.filter (fun j ->
	j > p.n) (chl p.m i))) > 1) (of_int (p.r + p.n)))
    
(* Is p epimorphic: no root is idle and no two roots are partners *)
let is_epi p =
  (* Idle roots? *)  
  not (Int_set.exists (fun i -> chl p.m i = Int_set.empty) (of_int p.r))
  &&
    (* Sites or nodes with more than one root? *)
    not (Int_set.exists (fun j ->
      (Int_set.cardinal (Int_set.filter (fun i ->
	i < p.r) (prn p.m j))) > 1) (of_int (p.n + p.s)))

(* Is p guarded: no root has sites as children *)
let is_guard p =
  Int_set.for_all (fun i ->
    Int_set.is_empty (Int_set.filter (fun j ->
      j > p.n) (chl p.m i))) (of_int p.r)

(* Functions only for nodes *)
(* is a child of b in p? (a and b columns)*)
let is_chl p a b = p.m.{p.r + b, a}

(* is a parent of b in p? (a and b columns)*)
let is_prn p a b = p.m.{p.r + a, b} 

(* is a a descendant of b? m is the transitive closure of the nodes-submatrix*)
let is_desc m a b = m.{b, a}

(* USE TRANSITIVE CLOSURE *)
(* Get the set of descendants (columns) of a set of rows.
   Not exposed. 
   raise Assert_failure *)
let desc p rs = 
  let rec aux acc rows =
    let chl_set = Int_set.fold (fun i acc ->
      Int_set.union (chl p.m i) acc) rows Int_set.empty   
    in
    if chl_set = Int_set.empty then acc
    else aux (Int_set.union chl_set acc) (Int_set.filter (fun i ->
      i < p.r + p.n) (off p.r chl_set))
  in aux Int_set.empty rs

(* Get the set of ancestors (rows) of a set of columns.
   Not exposed. 
   raise Assert_failure   *)
let anc p cs =
  let rec aux acc cols =
    let prn_set = Int_set.fold (fun j acc ->
      Int_set.union (prn p.m j) acc) cols Int_set.empty   
    in
    if prn_set = Int_set.empty then acc
    else aux (Int_set.union prn_set acc) (Int_set.filter (fun i ->
      i >= 0) (off (-p.r) prn_set))
  in aux Int_set.empty cs

(* Get the set of siblings (nodes) of a node (column)
   Not exposed. *)
let siblings p j = 
  let par = Int_set.filter (fun i -> i >= p.r) (prn p.m j) in
  Int_set.remove j (Int_set.fold (fun i acc ->
    Int_set.union acc (Int_set.filter (fun j -> 
      j < p.n) (chl p.m i))) par Int_set.empty)

(* Dual *)
let partners p j =
  let  chi = Int_set.filter (fun j -> j < p.n) (chl p.m (j + p.r)) in
  Int_set.remove j (Int_set.fold (fun j acc ->
    Int_set.union acc (Int_set.filter (fun j -> 
      j >= 0) (off (-p.r) (prn p.m j)))) chi Int_set.empty)

(* Equivalence class *)
(*let part_root_class p = 
  Int_set.fold (fun r acc -> 
    let c = chl p.m r in
    List.fold_left (fun acc s ->
      if Int_set.equal c (chl p.m (Int_set.singleton s)) then begin
	(Int_set.add r s) :: acc
      end else begin 
	s :: acc
      end
    ) [] acc
  ) (of_int p.r) [] *)

(* Dual *)

(* Build the decomposition of target t given pattern p and isomorphism over
   nodes i: t -> p. The result is context c, id, d, and nodes in c and d 
   expressed as rows of t. Pattern p is mono and epi.
   See page 76, proposition 4.2.4. *)
let decomp t p iso =
  (* Nodes (rows) of target *)
  let v_t = off t.r (of_int t.n) in
  (* Nodes (rows) of target used for p' *)
  let v_p = off t.r (codom iso) in
  (* Nodes (rows) of target used for context c *)
  let v_c = 
    Int_set.filter (fun i ->
      i > t.r) (Int_set.diff (anc t (codom iso)) v_p) in
  (* Nodes (rows) of target used for argument d *) 
  let v_d = Int_set.diff v_t (Int_set.union v_c v_p) in
  (* Iso from nodes or roots (rows) of target to indices from 0.
     Domain is the set of elements being in c and having a child
     in d. Codomain is the set of sites. *)
  let iso_id, _ =
    Int_set.fold (fun i (iso, k) ->
      Int_set.fold (fun j (acc, k) ->
        if (j > t.n) || Int_set.mem (j + t.r) v_d
        then (Iso.add (i, k) acc, k + 1) 
        else (acc, k)) (chl t.m i) (iso, k)
    ) (Int_set.union v_c (of_int t.r)) (Iso.empty, 0) in
  (*printf "iso_id: %s\n" (string_of_iso iso_id);*)
  (* Interface id *)
  let j = Iso.cardinal iso_id in
    (* if iso_id = Iso.empty then 0 else j + 1 in *) 
  (* Context c *)      
  let (r_c, n_c, s_c) = (t.r, Int_set.cardinal v_c, p.r + j) in
  (*printf "context c = (%d, %d, %d)\n" r_c n_c s_c;*)
  (* Parameter d *)
  let (r_d, n_d, s_d) = (p.s + j, Int_set.cardinal v_d, t.s) in
  (*printf "parameter d = (%d, %d, %d)\n" r_d n_d s_d;*)
  (* Context c matrix *)
  let m_c = make_0 (r_c + n_c) (n_c + s_c)
  (* Iso from columns in c to rows in t *)
  and iso_c = inverse (fix_num v_c) in
    for i = 0 to r_c + n_c - 1 do
    let new_i =
      if i < r_c then i else get_i (i - r_c) iso_c in
    (*printf "new_i = %d\n" new_i;*)
    for j = 0 to n_c + p.r - 1 do
      let new_j =
        if j < n_c then
          (* Node *)
          (get_i j iso_c) - t.r
        else (*if j < (n_c + p.r) then*)
          (* Site to pattern *)
	  Int_set.choose (apply (Int_set.filter (fun x ->
	    x < p.n) (chl p.m (j - n_c))) iso)
	(*else
          (* Site to id *)
          (get_inv_i (j - n_c - p.r) iso_id) - t.r*) in
      (*printf "new_j = %d\n" new_j;*)
      m_c.{i,j} <- t.m.{new_i,new_j}
    done
  done;
  (* edges to j *)
  Iso.iter (fun (i, j) ->
    let new_i = if i < r_c then i else get_i (i - r_c) iso_c in 
    m_c.{new_i, j + n_c + p.r} <- 1) iso_id;
  (* Parameter d matrix*)
  let m_d = make_0 (r_d + n_d) (n_d + s_d)
  (* Iso from columns in d to columns in t *)
  and iso_d =  inverse (fix_num (off (-t.r) v_d)) in
  for i = 0 to r_d + n_d - 1 do
    let new_i =
      if i < p.s then
        (* Root to pattern *)
        (*NOT TRUE!! p mono -> Sites in p have one parent *)
        (get_i ((Int_set.choose (prn p.m (i + p.n))) - p.r) iso) + t.r
      else if i < r_d then
        (* Root to id *)
        get_inv_i (i - p.s) iso_id
      else  
        (* Node *)
        (get_i (i - r_d) iso_d) + t.r in
    for j = 0 to n_d + s_d - 1 do
      let new_j = 
	if j < n_d then
          (* Node *)
          get_i j iso_d
	else
          (* Site *)  
          (j - n_d) + t.n  in
      m_d.{i,j} <- t.m.{new_i,new_j}
    done
  done;  
  ({r = r_c; n = n_c; s = s_c; m = m_c},
   elementary_id j,
   {r = r_d; n = n_d; s = s_d; m = m_d},
   Iso.fold (fun (x, y) acc ->
     Iso.add (y - t.r, x) acc) iso_c Iso.empty, inverse iso_d)

(* Parallel composition of n ions *)
let elementary_ions n =
  Int_set.fold (fun _ acc -> tens elementary_ion acc) (of_int n) id0

(* Construct the placing for a level. Firts two arguments are columns while
   last two are rows. *)
let build_phi sites sites_id roots roots_id p =
  (* Generate an map with an offset in the codomain *) 
  let fix_off target offset =
    Array.to_list (Array.mapi (fun index i ->
      (i, index + offset)) (Array.of_list target)) in
  (* Iso from inputs to phi indices *) 
  let iso_roots = fix_num roots
  and map_roots_id = fix_off roots_id (Int_set.cardinal roots) in
  (* Apply isos to parent sets *)
  let map_par (s : Int_set.t) = 
    fst (Int_set.fold (fun j (acc, map) ->
      (* parents in roots and in roots_id *) 
      let p_r, p_s =
	Int_set.partition (fun i -> Int_set.mem i roots) (prn p.m j) in
      (* map p_s to p_s0 and update map_roots_id *)
      let p_s0, new_map =
        Int_set.fold (fun i (acc, m) ->
          ((try (List.assoc i m) :: acc with _ -> acc),
           List.remove_assoc i m)) p_s ([], map) in
      (acc @ [(Int_set.elements (apply p_r iso_roots)) @ p_s0], new_map)
    ) s ([], map_roots_id)) in
  (* parents of sites *)  
  let (prn_sites : int list list) = 
       map_par sites  
  (* parents of sites_id *)  
  and (prn_id : int list list) = 
       map_par sites_id in   
  parse_placing (prn_sites @ prn_id)
    ((Int_set.cardinal roots) + (List.length roots_id))  

(* Compute the levels of p. Indeces are columns. *)
let levels p =
  (* leaves = columns already in a level*) 
  let rec loop p sites sites_id res leaves leaves_init =
    let roots, roots_id, prop =
      Int_set.fold (fun j (acc, l, s) ->
        (* Partition the set of parents of a site *)
        let keep, discard =
          Int_set.partition (fun i ->
            Int_set.subset (chl p.m i) leaves) (prn p.m j) in
	(* Keep only new roots *)    
        let new_keep = Int_set.filter (fun i ->
          not (Int_set.mem (i - p.r) leaves)) keep in
	 if Int_set.is_empty discard then
          (Int_set.union new_keep acc, (Int_set.elements discard) @ l, s)
        else
          (Int_set.union new_keep acc, (Int_set.elements discard) @ l,
           Int_set.add j s)
      ) (Int_set.union sites sites_id) (leaves_init, [], Int_set.empty) in
    (* If no nodes in parents *)         
    if Int_set.is_empty
      (Int_set.filter (fun i ->
	i >= p.r) (Int_set.union roots (set_of_list roots_id)))
    then
      (* Build topmost placing and return levels *)
      (build_phi sites sites_id (of_int p.r) [] p, res)
    else
      (* build current level and iterate loop *)
      (* placing *)
      let phi = build_phi sites sites_id roots roots_id p
      (* nodes (columns) not in leaves *)
      and ions = Int_set.fold (fun i acc ->
          if (i >= p.r) && not (Int_set.mem (i - p.r) leaves) then
            Int_set.add (i - p.r) acc
          else acc) roots Int_set.empty in  
      loop p ions prop ((ions, Int_set.cardinal prop, phi) :: res)
	(Int_set.union ions leaves) Int_set.empty in    
  let sites = off p.n (of_int p.s) 
  and leaves = zero_rows p.m in
  loop p sites Int_set.empty [] sites leaves	 

(* Compute three strings to build a dot representation.*)
(* USE CSS *)
let get_dot p =
  (
    (* Root shapes *)
    Int_set.fold (fun i buff ->
      buff ^ (sprintf "r%d [label=\"%d\", style=dashed, shape=box, width=.28, height=.18];\n" i i)
    ) (of_int p.r) "",
  (* Site shapes *)
    Int_set.fold (fun i buff ->
      buff ^ (sprintf
		"s%d [label=\"%d\", style=\"filled,dashed\", shape=box, fillcolor=gray, width=.28, height=.18];\n"
		i i)) (of_int p.s) "",
  (* ranks *)
    String.concat "" (List.map (fun (s, _, _) ->
      sprintf "{rank=same; %s};\n"
	(String.concat "; " (List.map (fun i ->
	  sprintf "v%d" i) (Int_set.elements s)))) (snd (levels p))),  
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
  Int_set.filter (fun j ->
    not (Int_set.is_empty (Int_set.filter (fun j ->
	     j >= b.n) (chl b.m (j + b.r))))) (of_int b.n)

(* Dual *)
let nodes_root_par b = 
  Int_set.filter (fun j ->
    not (Int_set.is_empty (Int_set.filter (fun i ->
	     i < b.n) (prn b.m j)))) (of_int b.n)

let match_leaves t p =
  (* forbid pairs (0,n>0) children *)
  let leaves_p =  Int_set.filter (fun x -> x >= 0) (off (-p.r) (zero_rows p.m)) 
  and non_leaves_t = Int_set.diff (of_int t.n) (Int_set.filter (fun x ->
    x >= 0) (off (-t.r) (zero_rows t.m))) in
  Int_set.fold (fun i acc ->
    Iso.union acc (Int_set.fold (fun j acc ->
      Iso.add (i,j) acc) non_leaves_t Iso.empty)) leaves_p Iso.empty

let match_orphans t p =
  (*forbid pairs (0,n>0) roots *)
  let orphans_p = Int_set.filter (fun x -> x < p.n) (zero_cols p.m) 
  and non_orphans_t = Int_set.diff (of_int t.n) (Int_set.filter (fun x ->
    x < t.n) (zero_cols t.m)) in
  Int_set.fold (fun i acc ->
    Iso.union acc (Int_set.fold (fun j acc ->
      Iso.add (i,j) acc) non_orphans_t Iso.empty)) orphans_p Iso.empty 

(* Add easy blocking pairs, Iso checking of the siblings and partners is left out *)
let match_sites t p =
  let n_p =  nodes_site_child p 
  and n_t = of_int t.n in
  Int_set.fold (fun i acc ->
    Iso.union acc (Int_set.fold (fun j acc ->
      if (Int_set.cardinal (partners t j)) < (Int_set.cardinal (partners p i)) then
	((*printf "match_sites (%d,%d)\n" i j;*)
	Iso.add (i,j) acc)
      else
	acc) n_t Iso.empty)) n_p Iso.empty

let match_roots t p =
  let n_p =  nodes_root_par p 
  and n_t = of_int t.n in
  Int_set.fold (fun i acc ->
    Iso.union acc (Int_set.fold (fun j acc ->
      if (Int_set.cardinal (siblings t j)) < (Int_set.cardinal (siblings p i)) then
	((*printf "match_sites (%d,%d)\n" i j;*)
	Iso.add (i,j) acc)
      else
	acc) n_t Iso.empty)) n_p Iso.empty

(* return a set of nodes (columns). Roots are discarded. *)
let prn_set p js =
  Int_set.fold (fun j acc ->
    Int_set.union acc (Int_set.filter (fun x -> x >= 0 ) (off (-p.r) (prn p.m j))))
    js Int_set.empty

(* check if iso i:p -> t is valid *)
let is_match_valid t p t_trans iso = 
  (* check SITES *)
  let check_sites t p iso =
    let n_t_sites = 
      Int_set.fold (fun j acc ->
	Int_set.union acc (chl t.m j)) (* diff sites t ??? NO *) 
	(off t.r (codom iso)) Int_set.empty in
    Int_set.for_all (fun c -> 
      (* is there a set of sites with the same parent set? *)
      let prn_c = 
	Int_set.inter (codom iso) (off (-t.r) (prn t.m c)) in
      (* construct a candidate set of sites *)
      let candidate = 
	Int_set.fold (fun s acc ->
	  let prn_s =
	    apply (Int_set.filter (fun x ->
	      x >= 0) (off (-p.r) (prn p.m s))) iso in
	  if Int_set.subset prn_s prn_c then
	    Int_set.union prn_s acc
	  else
	    acc) (off (p.n) (of_int p.s)) Int_set.empty in
      Int_set.equal candidate prn_c)
      (Int_set.diff n_t_sites (codom iso)) (* diff codom iso ??? YES *)
  (* check ROOTS (dual) *)
  and check_roots t p iso =
    let n_t_roots = (* rows *)
      Int_set.fold (fun j acc ->
	Int_set.union acc (prn t.m j)) (* diff roots t ??? NO *) 
	(codom iso) Int_set.empty in
    Int_set.for_all (fun _p -> 
      (* is there a set of roots with the same child set? *)
      let chl_p = (* cols *)
	Int_set.inter (codom iso) (chl t.m _p) in
      (* construct a candidate set of roots *)
      let candidate = (* cols *)
	Int_set.fold (fun r acc ->
	  let chl_r = 
	    apply (Int_set.filter (fun x ->  x < p.n) (chl p.m r)) iso in
	  if Int_set.subset chl_r chl_p then
	    Int_set.union chl_r acc
	  else
	    acc) (of_int p.r) Int_set.empty in
      Int_set.equal candidate chl_p)
      (Int_set.diff n_t_roots (off t.r (codom iso))) (* diff codom iso ??? YES *)
  (* check TRANS *)
  and check_trans t p t_trans iso =
   (* check if there is a node child of codomain, outside codomain, such that
      one of its children in trans is in codomain *)
    not (Int_set.exists (fun c ->
      Int_set.exists (fun t -> 
	Int_set.mem t (codom iso)) (chl t_trans c))
	   (Int_set.diff (Int_set.fold (fun x acc ->
	     Int_set.union acc (Int_set.filter (fun j -> 
	       j < t.n) (chl t.m x))) (off t.r (codom iso)) Int_set.empty)
	      (codom iso))) in
  (check_sites t p iso) && (check_roots t p iso) && (check_trans t p t_trans iso)

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

	
