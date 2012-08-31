open Base

open Printf

open Matrix

(* Type for concrete place graphs. The elements are roots, nodes, sites and
   adjacency matrix *)
type pg = {r: int; n: int; s: int; m: bmatrix}

(* Raised by comp. The elements are (sites, roots) *)
exception COMP_ERROR of (int * int) 

(* String representation *)
let string_of_pg p =
  sprintf "%d %d %d\n%s\n" p.r p.n p.s (to_string p.m)

(* Representation for match *)
let match_string p = (p.r, p.s, to_string p.m)

(* Apply isomorphism *)  
let apply_iso i p =
  {r = p.r;
	 n = p.n;
	 s = p.s;
	 m = Matrix.apply_iso i p.m p.r}

(* Elementary place graphs *)
let elementary_id n =
  {r = n;
   n = 0;
   s = n;
   m = diag n}
  
let elementary_merge n =
  {r = 1;
   n = 0;
   s = n;
   m = {Matrix.r = 1; c = n; Matrix.m = Array.make_matrix 1 n true}}
  
let elementary_split n =
  {r = n;
   n = 0;
   s = 1;
   m = {Matrix.r = n; c = 1; Matrix.m = Array.make_matrix n 1 true}}

let id0 =	elementary_id 0

let one = elementary_merge 0

let zero = elementary_split 0

let elementary_sym m n =
  {r = m + n;
   n = 0;
   s = m + n;
   m = stack (append (make n m) (diag n)) (append (diag m) (make m n))}
  
let elementary_ion =
  {r = 1;
   n = 1;
   s = 1;
   m = diag 2}

(* Parse a placing *)
let parse_placing l r =
  let v = parse_vector l r in
  {r = r;
   n = 0; 
   s = v.c;
   m = v}
  
(* Tensor product: A x B (indeces in the right handside are increased) *)
let tens a b =
  let (m_s, m_n, m_v, m_r) = split a.m a.r a.n
  and (n_s, n_n, n_v, n_r) = split b.m b.r b.n in
  let res = 
    stack
    (append m_s (append (make a.r b.n) (append m_n (make a.r b.s))))
    (stack
      (append (make b.r a.n) (append n_s (append (make b.r a.s) n_n)))
      (stack
        (append m_v (append (make a.n b.n) (append m_r (make a.n b.s))))
        (append (make b.n a.n) (append n_v (append (make b.n a.s) n_r))))) in
  {r = a.r + b.r;
   n = a.n + b.n;
   s = a.s + b.s;
   m = res}

(* Composition: G o F (indeces in the right handside are increased) *)
let comp g f =
  if g.s = f.r then
    let (a, b, _, _) = split g.m (g.r + g.n) g.n
    and (c, _, d, _) = split f.m f.r (f.n + f.s) in
    let res =
      stack (append a (mul b c)) (append (make f.n g.n) d) in
    {r = g.r;
     n = g.n + f.n;
     s = f.s;
     m = res}
  else raise (COMP_ERROR (g.s, f.r))

(* Is p an identity? *)
let is_id p =
	match p with
	| {r = x; n = 0; _} -> p.m = diag x
	| _ -> false

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
    Int_set.filter (fun i -> i > t.r) (Int_set.diff (anc t (codom iso)) v_p) in
  (* Nodes (rows) of target used for argument d *) 
  let v_d = Int_set.diff v_t (Int_set.union v_c v_p) in
  (* Iso from nodes or roots (rows) of target to indeces from 0.
     Domain is the set of elements being in c and having a child
     in d. Codomain is the set of sites. *)
  let (iso_id, j) =
    Int_set.fold (fun i (iso, k) ->
      Int_set.fold (fun j (acc, k) ->
        if (j > t.n) || Int_set.mem (j + t.r) v_d
        then (Iso.add (i, k) acc, k + 1) 
        else (acc, k)) (chl t.m i) (iso, k)) v_c (Iso.empty, 0) in
  (* Interface id *)
  let j = if iso_id = Iso.empty then 0 else j + 1 in 
  (* Context c *)      
  let (r_c, n_c, s_c) = (t.r, Int_set.cardinal v_c, p.r + j) in   
  (* Parameter d *)
  let (r_d, n_d, s_d) = (p.s + j, Int_set.cardinal v_d, t.s) in
  (* Context c matrix *)
  let m_c = make (r_c + n_c) (n_c + s_c)
  (* Iso from columns in c to rows in t *)
  and iso_c = inverse (fix_num v_c) in
  for i = 0 to m_c.Matrix.r - 1 do
    let new_i =
      if i < r_c then
        (* Root *)
        i
      else
        (* Node *)
        get_i (i - r_c) iso_c in
    for j = 0 to m_c.c - 1 do
      let new_j =
        if j < n_c then
          (* Node *)
          (get_i j iso_c) - t.r
        else if j < (n_c + p.r) then
          (* Site to pattern *)
          (* p epi -> Roots in p have one child *)
          get_i (Int_set.choose (chl p.m (j - n_c))) iso 
        else
          (* Site to id *)
          (get_inv_i (j - n_c - p.r) iso_id) - t.r in
        m_c.Matrix.m.(i).(j) <- t.m.Matrix.m.(new_i).(new_j)
    done
  done;  
  (* Parameter d matrix*)
  let m_d = make (r_d + n_d) (n_d + s_d)
  (* Iso from columns in d to columns in t *)
  and iso_d =  inverse (fix_num (off (-t.r) v_d)) in
  for i = 0 to m_d.Matrix.r - 1 do
    let new_i =
      if i < p.s then
        (* Root to pattern *)
        (* p mono -> Sites in p have one parent *)
        (get_i ((Int_set.choose (prn p.m (i + p.n))) - p.r) iso) + t.r
      else if i < r_d then
        (* Root to id *)
        get_inv_i (i - p.s) iso_id
      else  
        (* Node *)
        (get_i (i - r_d) iso_d) + t.r in
    for j = 0 to m_d.c - 1 do
      let new_j = 
       if j < n_d then
        (* Node *)
        get_i j iso_d
       else
        (* Site *)  
        (j - n_d) + t.n  in
      m_d.Matrix.m.(i).(j) <- t.m.Matrix.m.(new_i).(new_j)
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
  let (prn_sites : int list list) = map_par sites  
  (* parents of sites_id *)  
  and (prn_id : int list list) = map_par sites_id in   
  parse_placing (prn_sites @ prn_id)
    ((Int_set.cardinal roots) + (List.length roots_id))  

(* Compute the levels of p. Indeces are columns. *)
let levels p =
  (* leaves = columns already in a level*) 
  let rec loop p sites sites_id res leaves = 
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
      ) (Int_set.union sites sites_id) (Int_set.empty, [], Int_set.empty) in  
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
	  and ions =
	    Int_set.fold (fun i acc ->
          if (i >= p.r) && not (Int_set.mem (i - p.r) leaves) then
            Int_set.add (i - p.r) acc
          else acc) roots Int_set.empty in  
	  loop p ions prop ((ions, Int_set.cardinal prop, phi) :: res)
	    (Int_set.union ions leaves) in    
  let sites = off p.n (of_int p.s) in  
  loop p sites Int_set.empty [] sites	 

(* Compute three strings to build a dot representation.*)
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
  for i = 0 to p.m.Matrix.r - 1 do
    for j = 0 to p.m.Matrix.c - 1 do
      if p.m.Matrix.m.(i).(j) then
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
        out := !out ^ (sprintf "%s -> %s;\n" new_i new_j)
      else ()
    done
  done;
  !out)
  
(* Set of nodes children of a root. Nodes are counted from 0 to n-1. *)
(*let children_of_roots (Pg (r, n, _, m)) =
  let root_m = (* r x n submatrix: node children of a root *)
		Array.map (fun r -> Array.sub r 0 n) (Array.sub m.m 0 r)
	in Array.fold_left (fun acc r ->
					let tmp =
						Array.mapi (fun i v -> (i, v)) r
					in Array.fold_left (fun set (i, v) ->
									if v
									then Int_set.add i set
									else set)
						acc tmp)
		Int_set.empty root_m

(* Set of nodes parents of a site. Nodes are counted from 0 to n-1. *)
let parents_of_sites (Pg (r, n, s, m)) =
	let site_m = (* n x s submatrix: node parents of a site *)
		Array.map (fun r -> Array.sub r n s) (Array.sub m.m r n)
	in Array.fold_left (fun acc (i, r) ->
					if List.exists (fun i -> i = true) (Array.to_list r)
					then Int_set.add i acc
					else acc)
		Int_set.empty (Array.mapi (fun i r -> (i, r)) site_m)*)

(* Set of nodes having a parent in common with node i. Nodes and i are     *)
(* counted from 0 to n-1.                                                  *)
(*let siblings (Pg (_, n, _, m)) i =
	let tmp_m = (* remove sites *)
		Array.map (fun r -> Array.sub r 0 n) m.m
	and p_set = parents_set i p (* rows *)
	in let out = Int_set.fold (fun p acc ->
						let chl =
							Array.fold_left (fun set (i, v) ->
											if v
											then Int_set.add i set
											else set)
								Int_set.empty (Array.mapi (fun i v -> (i, v)) (tmp_m.(p)))
						in Int_set.union chl acc)
			p_set Int_set.empty
	in Int_set.remove i out

(* Set of nodes having a child in common with node 1. Nodes and i are      *)
(* counted from 0 to n-1.                                                  *)
let partners p i =
	let m = adj p
	and n = num_n p
	and r = out_f p
	and s = in_f p
	in let tmp_m = (* remove roots *)
		trans (Array.sub m r n) n (n + s)
	and c_set = children_set (i + r) p (* cols, rows for tmp_m *)
	in let out = Int_set.fold (fun c acc ->
						let prn =
							Array.fold_left (fun set (i, v) ->
											if v
											then Int_set.add i set
											else set)
								Int_set.empty (Array.mapi (fun i v -> (i, v)) (tmp_m.(c)))
						in Int_set.union prn acc)
			c_set Int_set.empty
	in Int_set.remove i out*)

(* DEBUG *)
(*let _ = 
  let (a, b) = (elementary_ion, elementary_ion) in
  printf "%s%s" (string_of_pg a) (string_of_pg b);
  let t = tens a b in
  printf "tensor:\n%s" (string_of_pg t);
  printf "sym(1,1):\n%s" (string_of_pg (elementary_sym 1 1));
  printf "split(2):\n%s" (string_of_pg (elementary_split 2));
  let c = comp t (elementary_sym 1 1) in
  printf "comp1:\n%s" (string_of_pg c);
  let d = comp c (elementary_split 2) in
  printf "comp2:\n%s" (string_of_pg d);
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
  t.m.Matrix.m.(1).(3) <- true;
  t.m.Matrix.m.(2).(4) <- true;
  t.m.Matrix.m.(3).(5) <- true;
  t.m.Matrix.m.(4).(6) <- true;
  t.m.Matrix.m.(5).(7) <- true;
  t.m.Matrix.m.(6).(7) <- true;
  t.m.Matrix.m.(7).(9) <- true;
  t.m.Matrix.m.(8).(8) <- true;
  (* extra edge from 0 to 5 to debug levels *)
  t.m.Matrix.m.(1).(5) <- true;
  printf "pattern:\n%starget:\n%s" (string_of_pg p) (string_of_pg t);
  printf "anc = %s\n" (string_of_Int_set (anc t (set_of_list [0;1;4])));  
  let (c, id, d, v_c, v_d) = decomp t p (of_list [(0,0);(1,1);(2,4)]) in
  printf "c:\n%s" (string_of_pg c);
  printf "id:\n%s" (string_of_pg id);
  printf "d:\n%s" (string_of_pg d);
  printf "v_c = %s\n" (string_of_Int_set (dom v_c));
  printf "v_d = %s\n" (string_of_Int_set (dom v_d));
  let phi, l = levels t in
  printf "levels:\n%s\n%s\n" (string_of_pg phi)
    (String.concat "\n" (List.map (fun (a, b, c) ->
      sprintf "(%s, %d, %s)" (string_of_Int_set a) b  (string_of_pg c)) l));
  let (dot_r, dot_s, dot_a) = get_dot t in  
  printf "get_dot:\n%s%s%s" dot_r dot_s dot_a
*)  

	
