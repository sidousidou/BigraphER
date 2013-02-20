open Base
open Printf
open Minisat
  
type bg = {
  p : Place.pg;  (** Place graph *)
  l : Link.Lg.t; (** Link graph *)
  n : Base.Nodes.t; (** Set of nodes *)
}

type inter = | Inter of int * Link.Face.t

exception SHARING_ERROR
exception CTRL_ERROR of int * Link.Face.t
exception ISO_ERROR of int * int (* number of nodes, size domain *)
exception NO_MATCH
exception INF_MATCHES

  
let inner b = Inter (b.p.Place.s, Link.inner b.l)
  
let outer b = Inter (b.p.Place.r, Link.outer b.l)
    
let inter_equals (Inter (i, n)) (Inter (j, m)) =
  (i = j) && (Link.Face.equal n m)

let ord_of_inter (Inter (i, _)) = i

let face_of_inter (Inter (_, f)) = f

let string_of_inter (Inter (n, f)) =
  sprintf "<%d, %s>" n (Link.string_of_face f)

    
let id (Inter (m, i)) =
  {n = Nodes.empty;
   p = Place.elementary_id m;
   l = Link.elementary_id i;
  }   
  
let id_eps = 
  {n = Nodes.empty;
   p = Place.id0;
   l = Link.id_empty;
  } 
  
let merge n =
  {n = Nodes.empty;
   p = Place.elementary_merge n;
   l = Link.id_empty;
  }
  
let split n =
  {n = Nodes.empty;
   p = Place.elementary_split n;
   l = Link.id_empty;
  }
  
let one =
  {n = Nodes.empty;
   p = Place.one;
   l = Link.id_empty;
  }
  
let zero =
  {n = Nodes.empty;
   p = Place.zero;
   l = Link.id_empty;
  }
  
let sym (Inter (m, i)) (Inter (n, j)) =
  {n = Nodes.empty;
   p = Place.elementary_sym m n;
   l = Link.tens (Link.elementary_id i) (Link.elementary_id j) 0;
  }
  
let ion f c =
  if (arity c) <> (Link.Face.cardinal f) then
    raise (CTRL_ERROR (arity c, f))
  else
    {n = Nodes.singleton (0, c);
     p = Place.elementary_ion;
     l = Link.elementary_ion f;
    } 

let sub i o =
  {n = Nodes.empty;
   p = Place.id0;
   l = Link.elementary_sub i o;
  }
  
let closure i = sub i Link.Face.empty
  
let intro o = sub Link.Face.empty o

(* (l list of parents for each site) r roots *)  
let placing l r f =
  {n = Nodes.empty;
   p = Place.parse_placing l r;
   l = Link.elementary_id f;
  }
  
(* Empty link graph and no nodes in the place graph. *)
let is_plc b =
  (Link.Lg.equal b.l Link.id_empty) &&
  (Nodes.cardinal b.n = 0) &&
  (b.p.Place.n = 0)
  
(* Empty place graph and no nodes in the link graph. *)
let is_wir b =
  (b.p = Place.id0) && (Nodes.cardinal b.n = 0)
  
let is_id b =
  (Nodes.is_empty b.n) && (Place.is_id b.p) && (Link.is_id b.l)
  
let tens a b =
  {n = uplus a.n b.n;
   p = Place.tens a.p b.p;
   l = Link.tens a.l b.l (Nodes.cardinal a.n);
  }

let comp a b =
  {n = uplus a.n b.n;
   p = Place.comp a.p b.p;
   l = Link.comp a.l b.l (Nodes.cardinal a.n);
  }
 
let ppar a b =
  {n = uplus a.n b.n;
   p = Place.tens a.p b.p;
   l = Link.ppar a.l b.l (Nodes.cardinal a.n);
  }
  
let ppar_of_list bs =
  List.fold_left (fun acc b -> ppar acc b) id_eps bs
  
let par a b =
  let p = ppar a b in
  let out_p = outer p in 
  comp (tens (merge (ord_of_inter out_p))
          (id (Inter (0, face_of_inter out_p)))
    ) p
  
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
  
let string_of_bg b =
  sprintf "%s\n%s%s" (string_of_nodes b.n) (Place.string_of_pg b.p)
    (Link.string_of_lg b.l)    
  
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
  let (x, y) = ((Iso.cardinal i), (Nodes.cardinal b.n)) in
  if x = y then
    {n = apply_nodes b.n i;
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
          sprintf "r%d" i) (Int_set.elements (of_int ord))) @
        (List.map (fun (Link.Nam n) ->
          sprintf "o%s" n) (Link.Face.elements f)) in
      match ss with
       | [] -> ""
       | _ -> sprintf "{rank=source; %s};\n" (String.concat "; " ss)
      else
        let xs =
        (List.map (fun i ->
          sprintf "s%d" i) (Int_set.elements (of_int ord))) @
        (List.map (fun (Link.Nam n) ->
          sprintf "i%s" n) (Link.Face.elements f)) in
        match xs with
         | [] -> ""
         | _ -> sprintf "{rank=sink; %s};\n" (String.concat "; " xs) in 
  let inner_shp, outer_shp, hyp_shp, link_adj = Link.get_dot b.l
  and roots_shp, sites_shp, node_ranks, place_adj = Place.get_dot b.p
  and nodes_shp = get_dot b.n
  and rank_out = build_rank (outer b) true
  and rank_in = build_rank (inner b) false in
  sprintf "digraph \"%s\"{\n%s%s%s%s%s%s%s%s%s%s%s}"
      ide roots_shp outer_shp rank_out hyp_shp nodes_shp sites_shp
      node_ranks inner_shp rank_in place_adj link_adj

let decomp t p i_v i_e =
  let p_c, p_id, p_d, i_c, i_d = Place.decomp t.p p.p i_v in
  let l_c, l_d, l_id = Link.decomp t.l p.l i_v i_e i_c i_d
  and n_c, n_d = apply_nodes t.n i_c, apply_nodes t.n i_d in
  ({ p = p_c; l = l_c; n = n_c },
   { p = p_d; l = l_d; n = n_d },
   { p = p_id; l = l_id; n = Nodes.empty })

(* List of bigraphs. First one is the top level. *)
let levels b =
  let phi, ls = Place.levels b.p in
  let w, ids = Link.levels b.l (List.map (fun (ps, _, _) ->
    ports_of_nodes (Nodes.filter (fun (i, _) ->
      Int_set.mem i ps) b.n)) ls) in
  (tens {p = phi; n = Nodes.empty; l = Link.id_empty;}(* no need for tens *)
        {p = Place.id0; l = w; n = Nodes.empty;}) ::
    (List.map2 (fun (ions, n, psi) l ->
      tens (comp (tens (ppar_of_list (Int_set.fold (fun j acc ->
            (* find j in the node set *)
            let n_j = (* Not_found? *)
              snd (Nodes.choose (Nodes.filter (fun (i, _) ->
                i = j) b.n)) in
            let f = 
              Int_set.fold (fun i acc ->
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

  
(* Initialise a matrix of varibles for the SAT solver *)
let init_vars r c solver =
  let m = Array.make_matrix r c 0 in
  for i = 0 to r - 1 do
    for j = 0 to c -1 do
      m.(i).(j) <- solver#new_var 
    done;
  done;
 (* print_endline (String.concat "\n" (Array.to_list (Array.map (fun row ->
    sprintf "|%s|" (String.concat " " (Array.to_list (Array.map (fun x ->
      sprintf "%8d" x) row)))) m)));*)
  m

let add_bijection v n m solver =
  (* Add first constraint; TOTAL -- at least a TRUE for every row *)
  for i = 0 to n - 1 do
    (*printf "%s\n" (String.concat " V " (Array.to_list (Array.mapi (fun j _ ->
      sprintf "[%d,%d]" i j) v.(i))));*)
    solver#add_clause (List.map (fun v ->
      pos_lit v) (Array.to_list v.(i)))
  done;
  (* Add second constraint: INJECTIVE -- at most a TRUE for every row *)
  for i = 0 to n - 1 do
    for j = 0 to m - 2 do
      for k = j + 1 to m - 1 do
	(*printf "![%d,%d] V ![%d,%d]\n" i j i k;*)
        solver#add_clause [(neg_lit v.(i).(j)); (neg_lit v.(i).(k))]
      done;
    done;
  done;
  (* Add third constraint: SURJECTIVE -- on the codomain, at most a TRUE for
     every column *)
  for j = 0 to m - 1 do
    for i = 0 to n - 2 do
      for l = i + 1 to n - 1 do
	(*printf "![%d,%d] V ![%d,%d]\n" i j l j;*)
        solver#add_clause [(neg_lit v.(i).(j)); (neg_lit v.(l).(j))]
      done;
    done;
  done           

(* check clauses with only negation or empty *)
(* list of clauses in the form [!(e,e) | (n,n) | ...]*)
(*let clauses_iter l v w solver =
  List.iter (fun clause ->
  solver#add_clause (
    let (i, j) = List.hd clause in
    (*printf "\n!w[%d,%d] " i j;*)
    neg_lit w.(i).(j) :: (List.map (fun (i,j) ->
      (*printf "V v[%d,%d] " i j;*)
      pos_lit v.(i).(j)) (List.tl clause)))) l *)

(* Generates an iso from a matrix of assignments *)
let get_iso solver vars n m = 
  let res = ref [] in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      match solver#value_of vars.(i).(j) with
	| Minisat.True -> res := (i,j)::!res
	| _ -> ()
    done;
  done;
  List.fold_left (fun acc x ->
    Base.Iso.add x acc) Base.Iso.empty !res 

let add_blocking solver v n m w e f =
  let scan_matrix m r c =
    let blocking_clause = ref [] in
    for i = 0 to r - 1 do
      for j = 0 to c - 1 do
	match solver#value_of m.(i).(j) with
	  | Minisat.True -> 
	    ((*printf "!m[%d,%d] V " i j;*)
	     blocking_clause := (neg_lit m.(i).(j))::!blocking_clause)
	  | _ -> 
	    ((*printf "m[%d,%d] V " i j;*)
	     blocking_clause := (pos_lit m.(i).(j))::!blocking_clause)
      done;
    done;
    !blocking_clause in
  solver#add_clause ((scan_matrix v n m) @ (scan_matrix w e f))


let rec filter_loop solver t p v n m w e f t_trans = 
    solver#simplify;
    match solver#solve with
      | Minisat.UNSAT -> 
	begin
	 (* printf "NO_MATCH\n";*)
	 (* solver#print_stats;*)
	  raise NO_MATCH
	end
      | Minisat.SAT ->
	begin
	  (* printf "MATCH\n"; *)
	  (* solver#print_stats; *)
	  let iso_v, iso_e = get_iso solver v n m, get_iso solver w e f in
	  if (Place.is_match_valid t.p p.p t_trans iso_v) && 
	    (Link.is_match_valid t.l p.l iso_e) then
	    solver, v, n, m, w, e, f, t_trans
	  else
	    begin
	      add_blocking solver v n m w e f;
	      filter_loop solver t p v n m w e f t_trans
	    end   
	end 

(*isos from pattern(col) to target(col)*)
(* Return empty iso when no nodes in the pattern *)
(* TO FINISH:
   - add constraint for SITES )
   - add constraint for ROOTS
   - add constraint for TRANSITIVE CLOSURE *)
let aux_match t p  =
  let solver = new solver
  and m, n = t.p.Place.n, p.p.Place.n 
  and e , f = Link.Lg.cardinal (Link.close_edges p.l),
    Link.Lg.cardinal (Link.close_edges t.l) in
  let _,_,v,_ = Matrix.split t.p.Place.m t.p.Place.r t.p.Place.n in
  let t_trans = Matrix.trans v in 
  (* Aux function *)
  let iso_iter m iso =
    Iso.iter (fun (i, j) -> 
      (*printf "!m[%d,%d]\n" i j;*)
      solver#add_clause [(neg_lit m.(i).(j))]) iso in
  (* Iso between nodes *)
  let v = init_vars n m solver
  (* Iso between closed edges *)
  and w = init_vars e f solver in
  (* Add bijection over nodes *)
  (*printf "add_bijection v\n";*)
  add_bijection v n m solver;
  (* Add bijection over closed edges *)
  (*printf "add_bijection w\n";*)
  add_bijection w e f solver;
  let block_ctrl = union_list
    [ (* CONTROLS *)
      match_nodes t.n p.n;
      (* LEAVES *)
      Place.match_leaves t.p p.p;
      (* ORPHANS *)
      Place.match_orphans t.p p.p;
      (* SITES *)
      Place.match_sites t.p p.p;
      (* ROOTS *)
      Place.match_roots t.p p.p;
    ]  in
  (* Add fourth constraint: EDGES in the place graph and
                            HYPEREDGES in the link graph *)
  (*printf "Adding C4\n";*)
  List.iter (fun (i, l, j, k) ->
    (*printf "!v[%d,%d] V !v[%d,%d]\n" i j l k;*)
    solver#add_clause [(neg_lit v.(i).(j)); (neg_lit v.(l).(k))])
    ((Place.match_list t.p p.p) @ (Link.match_peers t.l p.l m n));
  (* Add blocking pairs *)
  let iso_ports, constraint_e, block_e_e = Link.match_edges t.l p.l block_ctrl
  and block_l_n, block_l_e = Link.match_links t.l p.l in   
  let blocking_pairs_v = Iso.union block_l_n block_ctrl in
  (*printf "Adding blocking pairs v\n";*)
  iso_iter v blocking_pairs_v;
  (*printf "Adding blocking pairs e\n";*)
  iso_iter w (Iso.union block_e_e block_l_e); 
  (*printf "Adding constraint e\n";*)
  List.iter (fun (e_i, e_j, i, j) ->
    (*printf "!v[%d,%d] V !v[%d,%d]\n" i j l k;*)
    solver#add_clause [(neg_lit w.(e_i).(e_j)); (neg_lit v.(i).(j))])
    constraint_e;
  (*printf "Adding %d clauses for iso ports\n" (List.length iso_ports);*)
  List.iter (fun ((e_i, e_j), iso) ->
    let lits = 
      List.map (fun (i,j) -> pos_lit v.(i).(j)) (Iso.elements iso) in
    solver#add_clause ((neg_lit w.(e_i).(e_j)) :: lits)) iso_ports;
  filter_loop solver t p v n m w e f t_trans

let occurs t p = 
  try
    if Nodes.cardinal p.n = 0 then
      true
    else
      (ignore (aux_match t p);
       true)
  with
    | NO_MATCH -> false

let occurrence t p =
  if Nodes.cardinal p.n = 0 then
    raise INF_MATCHES (* Also check idle edges *)
  else
    let s, v, n, m, w, e, f, _ = aux_match t p in
    (get_iso s v n m, get_iso s w e f)

let occurrences t p =
  if Nodes.cardinal p.n = 0 then
    raise INF_MATCHES (* Also check idle edges *)
  else
    try 
      let solver, v, n, m, w, e, f, t_trans = aux_match t p in
      let rec loop_occur res =
	add_blocking solver v n m w e f;
	try 
	  ignore (filter_loop solver t p v n m w e f t_trans);
	  loop_occur ( res @ [(get_iso solver v n m), (get_iso solver w e f)] )
	with
	  | NO_MATCH -> ((*solver#print_stats;*) res) in
      loop_occur [(get_iso solver v n m, get_iso solver w e f)]
    with
      | NO_MATCH -> []

(* TO FINISH *)
let id_equals id0 id1 =
  true

(* ADD ids *)
let equals a b =
  (inter_equals (inner a) (inner b)) && 
    (inter_equals (outer a) (outer b)) &&
    (Nodes.cardinal a.n = Nodes.cardinal b.n) &&
    (Link.Lg.cardinal a.l = Link.Lg.cardinal b.l) &&
    (try
       let i, e = occurrence a b in
       let c, d, _ = decomp a b i e in
       (is_id c) && (is_id d)
     with
       | NO_MATCH -> false)
 
(* DEBUG *)
(*let _ =
  let parse =
    List.fold_left (fun s f -> Link.Lg.add f s) Link.Lg.empty
  and parse_p =
    List.fold_left (fun s f -> Ports.add f s) Ports.empty
  and parse_n  = 
    List.fold_left (fun s (i,c,a) -> Nodes.add (i, Ctrl(c,a)) s) Nodes.empty in
  (* Example from page 82 *)
  let t = {
    p = {Place.r = 1; Place.n = 8; Place.s = 2; Place.m = Matrix.make 9 10};
    l = parse [{Link.o = Link.parse_face ["x"];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(0,0);(3,0);(4,0);(5,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(1,0);(7,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(2,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(6,0)]}];
    n = parse_n [(0, "A", 1); (1, "B", 1); (2, "B", 1);
                 (3, "A", 1); (4, "A", 1); (5, "A", 1);
                 (6, "B", 1); (7, "B", 1)];
    } in
  t.p.Place.m.Matrix.m.(0).(0) <- true;
  t.p.Place.m.Matrix.m.(1).(1) <- true;
  t.p.Place.m.Matrix.m.(1).(2) <- true;
  t.p.Place.m.Matrix.m.(1).(3) <- true;
  t.p.Place.m.Matrix.m.(2).(4) <- true;
  t.p.Place.m.Matrix.m.(3).(5) <- true;
  t.p.Place.m.Matrix.m.(4).(6) <- true;
  t.p.Place.m.Matrix.m.(5).(7) <- true;
  t.p.Place.m.Matrix.m.(6).(7) <- true;
  t.p.Place.m.Matrix.m.(7).(9) <- true;
  t.p.Place.m.Matrix.m.(8).(8) <- true;
  printf "target:\n%s\n" (string_of_bg t);
  printf "get_dot:\n%s\n" (get_dot t "target");
  let p = {
    p = {Place.r = 1; Place.n = 3; Place.s = 2; Place.m = Matrix.make 4 5};
    l = parse [{Link.o = Link.parse_face ["x"];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(0,0)]};
               {Link.o = Link.parse_face ["y"];
                Link.i = Link.parse_face [];
                Link.p = parse_p [(2,0)]};
               {Link.o = Link.parse_face [];
                Link.i = Link.parse_face ["z"];
                Link.p = parse_p [(1,0)]}];
    n = parse_n [(0, "A", 1); (1, "B", 1); (2, "A", 1)];
    } in
  p.p.Place.m.Matrix.m.(0).(0) <- true;
  p.p.Place.m.Matrix.m.(1).(1) <- true;
  p.p.Place.m.Matrix.m.(1).(3) <- true;
  p.p.Place.m.Matrix.m.(2).(2) <- true;
  p.p.Place.m.Matrix.m.(3).(4) <- true;
  printf "pattern:\n%s\n" (string_of_bg p);
  printf "get_dot:\n%s\n" (get_dot p "pattern");
*)

