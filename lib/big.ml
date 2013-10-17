open Base
open Printf
open Minisat
  
type bg = {
  p : Place.pg;  (** Place graph *)
  l : Link.Lg.t; (** Link graph *)
  n : Base.Nodes.t; (** Set of nodes *)
}

type inter = Inter of int * Link.Face.t

exception SHARING_ERROR
exception CTRL_ERROR of int * Link.Face.t
exception ISO_ERROR of int * int (* number of nodes, size domain *)
exception NO_MATCH
exception NODE_FREE
(*exception INF_MATCHES of Iso.t * Iso.t*)

let inner b = Inter (b.p.Place.s, Link.inner b.l)
  
let outer b = Inter (b.p.Place.r, Link.outer b.l)
    
let inter_equal (Inter (i, n)) (Inter (j, m)) =
  (i = j) && (Link.Face.equal n m)

let inter_compare (Inter (i, n)) (Inter (j, m)) =
  let x = i - j in
  match x with
  | 0 -> Link.Face.compare n m
  | _ -> x

let ord_of_inter (Inter (i, _)) = i

let face_of_inter (Inter (_, f)) = f

let string_of_inter (Inter (n, f)) =
  sprintf "<%d, %s>" n (Link.string_of_face f)

let to_string b =
  sprintf "%s\n%s%s" (Nodes.to_string b.n) (Place.to_string b.p)
    (Link.to_string b.l)    

let parse lines =
  let (r, n, s, e) =
    let a = 
      Array.of_list (Str.split (Str.regexp_string " ") (List.hd lines)) in
    (int_of_string a.(0), int_of_string a.(1), 
     int_of_string a.(2), int_of_string a.(3)) in
  let (s_n, lines_p, lines_l, _) =
    List.fold_left (fun (s_n, acc_p, acc_l, i) l ->
      match i with
      | 0 -> (s_n, acc_p, acc_l, 1)
      | 1 -> (l, acc_p, acc_l, 2)
      | _ -> begin
	if i < 2 + r + n then (s_n, acc_p @ [l], acc_l, i + 1)
	else if i < 2 + r + n + e then (s_n, acc_p, acc_l @ [l], i + 1)
	else (s_n, acc_p, acc_l, i + 1)
      end) ("", [], [], 0) lines in
  assert (List.length lines_l = e);
  let (l, h) = Link.parse lines_l in
  { n = Nodes.parse s_n h;
    p = Place.parse r n s lines_p;
    l = l;
  }

let id (Inter (m, i)) =
  { n = Nodes.empty ();
    p = Place.elementary_id m;
    l = Link.elementary_id i;
  }   
  
let id_eps = 
  { n = Nodes.empty ();
    p = Place.id0;
    l = Link.id_empty;
  } 
  
let merge n =
  { n = Nodes.empty ();
    p = Place.elementary_merge n;
    l = Link.id_empty;
  }
  
let split n =
  { n = Nodes.empty ();
    p = Place.elementary_split n;
    l = Link.id_empty;
  }
  
let one =
  { n = Nodes.empty ();
    p = Place.one;
    l = Link.id_empty;
  }
  
let zero =
  { n = Nodes.empty ();
    p = Place.zero;
    l = Link.id_empty;
  }
  
let sym (Inter (m, i)) (Inter (n, j)) =
  { n = Nodes.empty ();
    p = Place.elementary_sym m n;
    l = Link.tens (Link.elementary_id i) (Link.elementary_id j) 0;
  }
  
let ion f c =
  if (Ctrl.arity c) <> (Link.Face.cardinal f) then
    raise (CTRL_ERROR (Ctrl.arity c, f))
  else
    { n = Nodes.add (Nodes.empty ()) 0 c;
      p = Place.elementary_ion;
      l = Link.elementary_ion f;
    } 

let sub i o =
  { n = Nodes.empty ();
    p = Place.id0;
    l = Link.elementary_sub i o;
  }
  
let closure i = sub i Link.Face.empty
  
let intro o = sub Link.Face.empty o

(* (l list of parents for each site) r roots *)  
let placing l r f =
  { n = Nodes.empty ();
    p = Place.parse_placing l r;
    l = Link.elementary_id f;
  }
  
(* Empty link graph and no nodes in the place graph. *)
let is_plc b =
  (Link.Lg.equal b.l Link.id_empty) &&
    (b.n.Nodes.size = 0) &&
    (Place.is_plc b.p)
  
(* Empty place graph and no nodes in the link graph. *)
let is_wir b =
  (b.p = Place.id0) && (b.n.Nodes.size = 0)
  
let is_id b =
  (Nodes.is_empty b.n) && (Place.is_id b.p) && (Link.is_id b.l)
  
let tens a b =
  { n = Nodes.tens a.n b.n;
    p = Place.tens a.p b.p;
    l = Link.tens a.l b.l a.n.Nodes.size;
  }

let comp a b =
  { n = Nodes.tens a.n b.n;
    p = Place.comp a.p b.p;
    l = Link.comp a.l b.l a.n.Nodes.size;
  }
 
let ppar a b =
  { n = Nodes.tens a.n b.n;
    p = Place.tens a.p b.p;
    l = Link.ppar a.l b.l a.n.Nodes.size;
  }
  
let ppar_of_list bs =
  List.fold_left (fun acc b -> ppar acc b) id_eps bs

let tens_of_list bs = 
  List.fold_left (fun acc b -> tens acc b) id_eps bs
  
let par a b =
  let p = ppar a b in
  let out_p = outer p in 
  comp (tens (merge (ord_of_inter out_p))
          (id (Inter (0, face_of_inter out_p)))) 
    p
  
let par_of_list bs =
  List.fold_left (fun acc b -> par acc b) id_eps bs
  
let nest a b =
  let idx = id (Inter (0, face_of_inter (outer b))) in
  comp (ppar a idx) b
  
(* share f by psi in g check that psi is a placing *)
let share f psi g =
  if is_plc psi
  then comp (comp g (tens psi (id (Inter (0, face_of_inter (outer f)))))) f
  else raise SHARING_ERROR

(* not checking if f is a subset of outern b names *)
let close f b =   
  let g = Link.Face.diff (face_of_inter (outer b)) f 
  and n = ord_of_inter (outer b) 
  and cs = tens_of_list (List.map (fun n ->
    closure (Link.Face.singleton n)) (Link.Face.elements f)) in
  comp (tens cs (id (Inter (n, g)))) b
    
let is_mono b =
  (Place.is_mono b.p) && (Link.is_mono b.l)
  
let is_epi b =
  (Place.is_epi b.p) && (Link.is_epi b.l)

let is_guard b =
  (Place.is_guard b.p) && (Link.is_guard b.l)   

let is_solid b =
  (is_epi b) && (is_mono b) && (is_guard b)
  
(* TO DO *)
(*let latex_of_big = function | Bg (ns, p, l) -> "latex representation"*)
  
let apply_iso i b =
  let (x, y) = ((Iso.cardinal i), b.n.Nodes.size) in
  if x = y then
    { n = Nodes.apply_iso b.n i;
      p = Place.apply_iso i b.p;
      l = Link.apply_iso i b.l;
    }
  else raise (ISO_ERROR (y, x))

let get_dot b ide =
  let build_rank i flag =
    let ord = ord_of_inter i
    and f = face_of_inter i in 
    if  (ord = 0) && (Link.Face.is_empty f) then ""
    else if flag then
      let ss =
        (List.map (fun i ->
          sprintf "r%d" i) (IntSet.elements (IntSet.of_int ord))) @
        (List.map (fun (Link.Nam n) ->
          sprintf "o%s" n) (Link.Face.elements f)) in
      match ss with
       | [] -> ""
       | _ -> sprintf "{ rank=source; %s };\n" (String.concat "; " ss)
      else
        let xs =
        (List.map (fun i ->
          sprintf "s%d" i) (IntSet.elements (IntSet.of_int ord))) @
        (List.map (fun (Link.Nam n) ->
          sprintf "i%s" n) (Link.Face.elements f)) in
        match xs with
         | [] -> ""
         | _ -> sprintf "{ rank=sink; %s };\n" (String.concat "; " xs) in 
  let (inner_shp, outer_shp, hyp_shp, link_adj) = Link.get_dot b.l
  and (roots_shp, sites_shp, node_ranks, place_adj) = Place.get_dot b.p
  and nodes_shp = Nodes.to_dot b.n
  and rank_out = build_rank (outer b) true
  and rank_in = build_rank (inner b) false in
  sprintf "digraph \"%s\"{\n%s%s%s%s%s\n%s%s%s%s%s%s}"
      ide roots_shp outer_shp rank_out hyp_shp nodes_shp sites_shp
      node_ranks inner_shp rank_in place_adj link_adj

let decomp t p i_v i_e =
  let (p_c, p_d, p_id, i_c, i_d) = 
    Place.decomp t.p p.p i_v in
  let (l_c, l_d, l_id) = 
    Link.decomp t.l p.l i_v i_e i_c i_d
  and (n_c, n_d) = 
    (Nodes.filter_apply_iso t.n i_c, Nodes.filter_apply_iso t.n i_d) in
  ({ p = p_c; l = l_c; n = n_c },
   { p = p_d; l = l_d; n = n_d },
   { p = p_id; l = l_id; n = Nodes.empty () })

(*
(* List of bigraphs. First one is the top level. *)
let levels b =
  let (phi, ls) = Place.levels b.p in
  let (w, ids) = Link.levels b.l (List.map (fun (ps, _, _) ->
    Ports.of_nodes (Nodes.filter (fun (i, _) ->
      IntSet.mem i ps) b.n)) ls) in
  (tens {p = phi; n = Nodes.empty; l = Link.id_empty;}(* no need for tens *)
        {p = Place.id0; l = w; n = Nodes.empty;}) ::
    (List.map2 (fun (ions, n, psi) l ->
      tens (comp (tens (ppar_of_list (IntSet.fold (fun j acc ->
            (* find j in the node set *)
            let n_j = (* Not_found? *)
              snd (Nodes.choose (Nodes.filter (fun (i, _) ->
                i = j) b.n)) in
            let f = 
              IntSet.fold (fun i acc ->
                (sprintf "n%d_%d" j i) :: acc) (of_int (arity n_j)) [] in    
            (ion (Link.parse_face f) n_j) :: acc) ions []))
          {p = Place.elementary_id n; l = Link.id_empty; n = Nodes.empty;})
        {p = psi; l = Link.id_empty; n = Nodes.empty;})
      {p = Place.id0; l = l; n = Nodes.empty;}) ls ids)

let snf b =
  (* Size of identity in a level *)
  let k l = 
    assert (l.p.Place.r >= (Nodes.cardinal l.n));
    l.p.Place.r - (Nodes.cardinal l.n) in
  (* Returns a string representation of node n *) 
  let snf_of_node (j, c) b =
    (* in a level every edge is just a port and an outer name *)
    (string_of_ctrl c) ^
      (Link.string_of_face (Link.outer (Link.Lg.filter (fun e ->
	  not (Ports.is_empty (Ports.filter (fun (i, _) ->
            i = j) e.Link.p))) b.l))) in
  (* (ions || id) phi *)
  let snf_of_level l = 
    let ions =
      String.concat " || " ((List.map (fun x -> snf_of_node x l) (Nodes.elements l.n)) @
			       [ sprintf "id(%d)" (k l)])
    and phi =
      Place.snf_of_placing { Place.r = l.p.Place.r;
			     Place.n = 0;
			     Place.s = l.p.Place.s;
			     Place.m =
	  let  _, _, _, d = Matrix.split l.p.Place.m l.p.Place.n l.p.Place.n in
	  let  _, a, _, b = Matrix.split d (k l) 0 in
	  Matrix.stack b a } in
    sprintf "(%s) %s" ions phi in
  match  levels b with
  | h::tl -> String.concat "\n"
    ((sprintf "%s || %s" (Place.snf_of_placing h.p) (Link.snf_of_linking h.l)) :: 
	(List.map snf_of_level tl))
  | _ -> ""
    *)

(* Generates an iso from a matrix of assignments *)
let get_iso solver m =
  let iso = Iso.empty () in 
  Array.iteri (fun i r ->
    Array.iteri (fun j x ->
      match solver#value_of x with
      | Minisat.True -> Iso.add iso i j
      | Minisat.False | Minisat.Unknown -> ()
    ) r) m;
  iso

(************************** DEBUG *************************)
let string_of_SAT solver m =
  Iso.to_string (get_iso solver m)

type sat_vars = {
  iso_nodes : Minisat.var array array;
  z0_rows : Minisat.var array array;
  z0_cols : Minisat.var array array;
  iso_edges : Minisat.var array array;
  z1_rows : Minisat.var array array;
  z1_cols : Minisat.var array array;
  z2 : Minisat.var array array;
  iso_hyp : Minisat.var array array;
  z3 : Minisat.var array array;
}

let print_dump solver v =
  printf "------ ISO NODES\n\
          %s\n\
          ------ Z0 ROWS\n\
          %s\n\
          ------ Z0 COLUMNS\n\
          %s\n\
          ------ ISO EDGES\n\
          %s\n\
          ------ Z1 ROWS\n\
          %s\n\
          ------ Z1 COLUMNS\n\
          %s\n\
          ------ Z2\n\
          %s\n\
          ------ ISO HYPER\n\
          %s\n\
          ------ Z3\n\
          %s\n"
    (string_of_SAT solver v.iso_nodes) 
    (string_of_SAT solver v.z0_rows)
    (string_of_SAT solver v.z0_cols) 
    (string_of_SAT solver v.iso_edges)
    (string_of_SAT solver v.z1_rows)
    (string_of_SAT solver v.z1_cols)
    (string_of_SAT solver v.z2)
    (string_of_SAT solver v.iso_hyp)
    (string_of_SAT solver v.z3)
  
(**********************************************************)
   
let add_blocking solver v w =
  let scan_matrix m =
    snd
      (Array.fold_left (fun (i, acc) r ->
	(i + 1, 
	 snd 
	   (Array.fold_left (fun (j, acc) x ->
	     match solver#value_of x with
	     | Minisat.True -> (j + i, neg_lit x :: acc)
	     | Minisat.False -> (j + 1, pos_lit x :: acc) (* Check if this is really necessary*)
	     | Minisat.Unknown -> assert false) (0, acc) r
	   )
	)
       ) (0, []) m) in
  solver#add_clause ((scan_matrix v) @ (scan_matrix w))

let rec filter_loop solver t p vars t_trans = 
    solver#simplify;
    match solver#solve with
      | Minisat.UNSAT -> raise NO_MATCH
      | Minisat.SAT ->
	begin
	  let iso_v = get_iso solver vars.iso_nodes
	  (* and iso_e = get_iso solver w e f *) in
	  if (Place.check_match t.p p.p t_trans iso_v) then 
	    (solver, vars)
	  else begin
	    eprintf "Warning: invalid match not discarded by SAT. \
                     Removing it ...\n";
	    add_blocking solver vars.iso_nodes vars.iso_edges;
	    filter_loop solver t p vars t_trans
	  end   
	end 
	  
let add_c4 t p t_n p_n solver v =
  let (t_constraints, exc_clauses, js) = 
    Place.match_list t p t_n p_n in
  Cnf.post_conj_m exc_clauses solver v;
  (Array.of_list
     (List.fold_left (fun acc x ->
       (Cnf.post_tseitin x solver v) :: acc
      ) [] t_constraints),
   js)
  
let add_c5 t p t_n p_n solver v =
  let (clauses_l, js_l) = 
    Place.match_leaves t p t_n p_n
  and (clauses_o, js_o) = 
    Place.match_orphans t p t_n p_n in
  Cnf.post_conj_m (clauses_l @ clauses_o) solver v;
  IntSet.union js_l js_o

let add_c6 t p t_n p_n solver v =
  let (clauses_s, js_s) = 
    Place.match_sites t p t_n p_n
  and (clauses_r, js_r) = 
    Place.match_roots t p t_n p_n in
  Cnf.post_conj_m (clauses_s @ clauses_r) solver v;
  IntSet.union js_s js_r

let add_c11 unmatch_v solver aux rc_v =
  let clauses  =
    IntSet.fold (fun i acc ->
      (List.map (fun z ->
	[Cnf.N_var (Cnf.V_lit z)]) rc_v) @ acc
    ) unmatch_v [] in
  Cnf.post_conj_m clauses solver aux

let add_c7 t p t_n p_n f aux rc_v solver v =
  let (clauses, js) = Link.match_edges t p t_n p_n in
  Cnf.post_conj_m clauses solver v;
  add_c11 (IntSet.diff (IntSet.of_int f) js) solver aux rc_v;  
  clauses

let add_c8 t p t_n p_n clauses solver v w =
  let constraints = Link.match_ports t p t_n p_n clauses in
  List.iter (fun x ->
    Cnf.post_impl x solver w v) constraints

let add_c10 t p solver v =
  Cnf.post_conj_m (Place.match_trans t p) solver v

(* Fix *)
let add_c9 t p t_n p_n solver v =
  let (r, c, constraints, b) = 
    Link.match_peers t p t_n p_n in
  let w = Cnf.init_aux_m r c solver in
  Cnf.post_conj_m b solver w;
  List.iter (fun x ->
    Cnf.post_impl x solver w v
  ) constraints;
  let (aux_bij_w_rows, z_roots) =
    Cnf.post_tot (Cnf.tot_fun r c 6 3) solver w in
  (w, aux_bij_w_rows, z_roots)

(* Compute isos from nodes in the pattern to nodes in the target *)
let aux_match t p t_trans =
  try
    let solver = new solver
    and (n, m) = (p.p.Place.n, t.p.Place.n) 
    and closed_p = Link.closed_edges p.l
    and closed_t = Link.closed_edges t.l in
    let (e, f) = (Link.Lg.cardinal closed_p,
		  Link.Lg.cardinal closed_t) in
    (* Iso between nodes *)
    let v = Cnf.init_aux_m n m solver
    (* Iso between closed edges *)
    and w = Cnf.init_aux_m e f solver in
    (* Add bijection over nodes *)
    let ((aux_bij_v_rows, _), 
	 (aux_bij_v_cols, rc_v)) =
      Cnf.post_bij (Cnf.bijection n m 6 3) solver v
    (* Add bijection over closed edges *)
    and ((aux_bij_w_rows, _), 
	 (aux_bij_w_cols, rc_w)) =
      Cnf.post_bij (Cnf.bijection e f 6 3) solver w
    (* Add Tseitin C4: ctrl, edges and degrees in the palce graphs.
       Return list of vectors of auxiliary vars. *)
    and (zs4, js0) = add_c4 t.p p.p t.n p.n solver v
    (* Add C5: orphans and leaves matching in the place graphs. *)
    and js1 = add_c5 t.p p.p t.n p.n solver v
    (* Add C6: sites and roots in the place graphs. *)
    and js2 = add_c6 t.p p.p t.n p.n solver v in
    (* Add C7: edges in the pattern are matched to edges in the target. *)
    let clauses = 
      add_c7 closed_t closed_p t.n p.n f aux_bij_w_cols rc_w solver w in
    (* Add C8: ports of matched closed edges have to be isomorphic. *)
    add_c8 closed_t closed_p t.n p.n clauses solver v w;
    (* Add C9: ports of matched open edges have to be isomorphic. 
       Return matrix from open edges in the pattern to non-empty edges in the
       target. *)
    let (w', aux_bij_w'_rows, _) = 
      add_c9 t.l p.l t.n p.n solver v in
    (* Add C10: block edges between unconnected nodes with sites and nodes with
       roots. *)
    add_c10 t.p p.p solver v;
    (* Block unmatchable columns *)
    let unmatch_v = 
      IntSet.diff 
	(IntSet.of_int m) 
	(IntSet.union js0 (IntSet.union js1 js2)) in
    add_c11 unmatch_v solver aux_bij_v_cols rc_v;
    let vars = {
      iso_nodes = v;
      z0_rows = aux_bij_v_rows;
      z0_cols = aux_bij_v_cols;
      iso_edges = w;
      z1_rows = aux_bij_w_rows;
      z1_cols = aux_bij_w_cols;
      z2 = zs4;
      iso_hyp = w';
      z3 = aux_bij_w'_rows;
    } in
    filter_loop solver t p vars t_trans
  with
  | Place.NOT_TOTAL -> raise NO_MATCH
  | Link.NOT_TOTAL -> raise NO_MATCH

(* true when p is not a match *)
let quick_unsat t p =
  (p.n.Nodes.size > t.n.Nodes.size) || 
    (Sparse.entries p.p.Place.nn > Sparse.entries t.p.Place.nn) ||
    (Nodes.not_sub p.n t.n) ||
    (IntSet.cardinal (Place.leaves p.p) > IntSet.cardinal (Place.leaves t.p)) ||
    (IntSet.cardinal (Place.orphans p.p) > IntSet.cardinal (Place.orphans t.p)) ||
    (Link.Lg.cardinal (Link.closed_edges p.l) > Link.Lg.cardinal (Link.closed_edges t.l)) ||
    (Link.max_ports p.l > Link.max_ports t.l)

let occurs t p = 
  try
    if p.n.Nodes.size = 0 then true
    else (if quick_unsat t p then false
      else (let t_trans = Sparse.trans t.p.Place.nn in
	    ignore (aux_match t p t_trans);
	    true))
  with
    | NO_MATCH -> false
    
let occurrence t p =
  if p.n.Nodes.size = 0 then raise NODE_FREE 
  else (if quick_unsat t p then raise NO_MATCH
    else (let t_trans = Sparse.trans t.p.Place.nn in
	  let (s, vars) = aux_match t p t_trans in
	  (get_iso s vars.iso_nodes, get_iso s vars.iso_edges)))

(* compute non-trivial automorphisms of b *)
let auto b =
  if b.n.Nodes.size = 0 then raise NODE_FREE 
  else begin
    let b_trans = Sparse.trans b.p.Place.nn
    and rem_id res = 
      List.filter (fun (i, e) ->
	not ((Iso.is_id i) && (Iso.is_id e))) res in
    rem_id (try 
	      let (s, vars) = aux_match b b b_trans in
	      let rec loop_occur res =
		add_blocking s vars.iso_nodes vars.iso_edges;
		try 
		  ignore (filter_loop s b b vars b_trans);
		  loop_occur (res @ [(get_iso s vars.iso_nodes), (get_iso s vars.iso_edges)])
		with
		| NO_MATCH -> res in
	      loop_occur [(get_iso s vars.iso_nodes, get_iso s vars.iso_edges)]
      with
      | NO_MATCH -> [])
  end
      
let clause_of_iso iso m =
  snd 
    (Array.fold_left (fun (i, acc) r ->
      (i + 1, snd 
	(Array.fold_left (fun (j, acc) x ->
	  if Iso.mem iso i j then (j + 1, neg_lit x :: acc)
	  else (j + 1, pos_lit x :: acc) (* Do we really need this? *)
	 ) (0, acc) r))
     ) (0, []) m)

let occurrences t p =
  if p.n.Nodes.size = 0 then raise NODE_FREE 
  else
    if quick_unsat t p then []
    else (try
	    (************************** DEBUG *************************)
	    printf "------- TARGET:\n%!\
                    %s\n\
                    ------- PATTERN:\n%!\
                    %s\n" (to_string t) (to_string p);
	    (**********************************************************)
	    let t_trans = Sparse.trans t.p.Place.nn in
	    let (s, vars) = aux_match t p t_trans in
	    print_dump s vars;
	    let autos = auto p in
	    let rec loop_occur res =
	      add_blocking s vars.iso_nodes vars.iso_edges;
	      (****************AUTOMORPHISMS****************)
	      let gen = 
		List.combine
		  (Iso.gen_isos (get_iso s vars.iso_nodes) (List.map fst autos)) 
		  (Iso.gen_isos (get_iso s vars.iso_edges) (List.map snd autos))  in
	      List.iter (fun (iso_i, iso_e) ->
		s#add_clause ((clause_of_iso iso_i vars.iso_nodes) @ 
				 (clause_of_iso iso_e vars.iso_edges))) gen;
	      (*********************************************)
	      try 
		ignore (filter_loop s t p vars t_trans);
		loop_occur (res @ 
			      [(get_iso s vars.iso_nodes), (get_iso s vars.iso_edges)])
	      with
	      | NO_MATCH -> res in
	    loop_occur [(get_iso s vars.iso_nodes, get_iso s vars.iso_edges)]
      with
      | NO_MATCH -> [])
    
let equal_SAT a b =
  try
    let solver = new solver in
    let n = a.p.Place.n
    and h = Link.Lg.cardinal a.l in
    let v_n = Cnf.init_aux_m n n solver
    and v_l = Cnf.init_aux_m h h solver in 
    let ((aux_bij_n_rows, _), 
	 (aux_bij_n_cols, _)) =
      Cnf.post_bij (Cnf.bijection n n 6 3) solver v_n
    and ((aux_bij_l_rows, _), 
	 (aux_bij_l_cols, _)) =
      Cnf.post_bij (Cnf.bijection h h 6 3) solver v_l in
    (* Place graph *)
    let (t_constraints, exc_clauses, cols0) = 
      Place.match_list_eq a.p b.p a.n b.n 
    and (c_rn, cols1) =
      Place.match_root_nodes a.p b.p a.n b.n
    and (c_ns, cols2) =
      Place.match_nodes_sites a.p b.p a.n b.n 
    and (clauses_l, js_l) = 
      Place.match_leaves a.p b.p a.n b.n
    and (clauses_o, js_o) = 
      Place.match_orphans a.p b.p a.n b.n in
    let cols =
      IntSet.union
	(IntSet.union js_o js_l)
	(IntSet.union cols0 (IntSet.union cols1 cols2)) in
    if IntSet.cardinal cols <> n then raise NO_MATCH;
    Cnf.post_conj_m (exc_clauses) solver v_n;
    Cnf.post_conj_m (c_rn @ c_ns) solver v_n;
    Cnf.post_conj_m (clauses_l @ clauses_o) solver v_n;
    let z = 
      Array.of_list (
	List.fold_left (fun acc x ->
	  (Cnf.post_tseitin x solver v_n) :: acc
	) [] t_constraints
      ) in
    (* Link graph *)
    let (clauses, b_pairs) = Link.match_list_eq a.l b.l a.n b.n in
    Cnf.post_conj_m (clauses @ b_pairs) solver v_l;
    let l_constraints = Link.match_ports_eq a.l b.l a.n b.n clauses in
    List.iter (fun x ->
      Cnf.post_impl x solver v_l v_n) l_constraints;
    solver#simplify;
    match solver#solve with
    | Minisat.UNSAT -> false
    | Minisat.SAT -> true
  with
  | NO_MATCH -> false

type bg_key = int * int * int * int * int

let key b = 
  (b.p.Place.r,
   b.p.Place.n,
   b.p.Place.s,
   Place.edges b.p,
   Link.Lg.cardinal b.l
  )

let equal a b =
  (a.n.Nodes.size = b.n.Nodes.size) &&
    (Link.Lg.cardinal a.l = Link.Lg.cardinal b.l) &&
    (inter_equal (inner a) (inner b)) && 
    (inter_equal (outer a) (outer b)) &&
    (Place.edges a.p = Place.edges b.p) &&
    (Place.deg_roots a.p = Place.deg_roots b.p) &&
    (Place.deg_sites a.p = Place.deg_sites b.p) &&
    (Nodes.equal a.n b.n) && 
    (Sparse.(=) a.p.Place.rs b.p.Place.rs) &&
    (* Placing or wiring *)
    if b.n.Nodes.size = 0 then
      (Place.equal_placing a.p b.p) && (Link.Lg.equal a.l b.l)
    else 
      equal_SAT a b

(* let compare a b = *)
(*   match (a.n.Nodes.size) - (b.n.Nodes.size) with *)
(*   | 0 ->  *)
(*     begin *)
(*       match (Link.Lg.cardinal a.l) - (Link.Lg.cardinal b.l) with *)
(*       | 0 ->  *)
(* 	begin *)
(* 	  match inter_compare (inner a) (inner b) with *)
(* 	  | 0 ->  *)
(* 	    begin *)
(* 	      match inter_compare (outer a) (outer b) with *)
(* 	      | 0 ->  *)
(* 		begin *)
(* 		  match (Place.edges a.p) - (Place.edges b.p) with *)
(* 		  | 0 ->  *)
(* 		    begin *)
(* 		      match Sparse.compare a.p.Place.rs b.p.Place.rs with *)
(* 		      | 0 -> *)
(* 			begin *)
(* 			  (\*placing or wiring *\) *)
(* 			  if b.n.Nodes.size = 0 then *)
(* 			    match Place.compare_placing a.p b.p with *)
(* 			    | 0 -> Link.Lg.compare a.l b.l *)
(* 			    | v -> v *)
(* 			  else if equal_SAT a b then 0 *)
(* 			  else compare a b *)
(* 			end *)
(* 		      | v -> v *)
(* 		    end *)
(* 		  | v -> v *)
(* 		end *)
(* 	      | v -> v  *)
(* 	    end *)
(* 	  | v -> v  *)
(* 	end *)
(*       | v -> v  *)
(*     end *)
(*   | v -> v  *)
