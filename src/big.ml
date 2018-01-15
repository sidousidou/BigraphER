open Base
open Printf
open Minisat

type t = {
  p : Place.t;      (* Place graph  *)
  l : Link.Lg.t;    (* Link graph   *)
  n : Nodes.t;      (* Set of nodes *)
}

type inter = Inter of int * Link.Face.t

type occ = Iso.t * Iso.t * Fun.t

exception SHARING_ERROR
exception COMP_ERROR of inter * inter
exception CTRL_ERROR of int * Link.Face.t
exception ISO_ERROR of int * int (* number of nodes, size domain *)
exception NO_MATCH
exception NODE_FREE

let inner b = Inter (b.p.Place.s, Link.inner b.l)

let outer b = Inter (b.p.Place.r, Link.outer b.l)

let inter_equal (Inter (i, n)) (Inter (j, m)) =
  (i = j) && (Link.Face.equal n m)

let ord_of_inter (Inter (i, _)) = i

let face_of_inter (Inter (_, f)) = f

let string_of_inter (Inter (n, f)) =
  "<" ^ (string_of_int n) ^ ", " ^ (Link.string_of_face f) ^ ">"

let to_string b =
  List.filter (fun x -> (String.compare "" x) <> 0)
    [ Nodes.to_string b.n;
      Place.to_string b.p;
      Link.to_string b.l ]
  |> String.concat "\n"

let size b =
  (Nodes.size b.n) + (Link.Lg.cardinal b.l)
  
let parse s =
  let lines = Base.parse_lines s in
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
        | _ ->
          (if i < 2 + r + n then
             (s_n, acc_p @ [l], acc_l, i + 1)
           else if i < 2 + r + n + e then
             (s_n, acc_p, acc_l @ [l], i + 1)
           else
             (s_n, acc_p, acc_l, i + 1)))
      ("", [], [], 0) lines in
  assert (List.length lines_l = e);
  let (l, s_n) = Link.parse ~links:lines_l ~nodes:s_n in
  { n = s_n;
    p = Place.parse ~regions:r ~nodes:n ~sites:s lines_p;
    l = l; }
  
let of_string s =
  let err = "Not a valid string representation of a bigraph" in
  let parse_p_header h =
    match Str.(split (regexp_string " ")) h with
    | r :: n :: s :: _ ->
      (int_of_string r, int_of_string n, int_of_string s)
    | _ -> invalid_arg err in
  let rec split_l n (a, b) =
    if n > 0 then match b with
      | x :: xs -> split_l (n - 1) (x :: a, xs)
      | [] -> assert false
    else (List.rev a, b) in
  match Base.parse_lines s with
  | n :: place_header :: rest ->
    let (regions, nodes, sites) = parse_p_header place_header in
    let (m, l) = split_l (regions + nodes) ([], rest) in
      { n = Nodes.of_string n;
        p = Place.parse ~regions ~nodes ~sites m;
        l = Link.of_string (String.concat "\n" l);
      }
  | _ -> invalid_arg err
    
let id (Inter (m, i)) =
  { n = Nodes.empty;
    p = Place.elementary_id m;
    l = Link.elementary_id i;
  }

let id_eps =
  { n = Nodes.empty;
    p = Place.id0;
    l = Link.id_empty;
  }

let merge n =
  { n = Nodes.empty;
    p = Place.elementary_merge n;
    l = Link.id_empty;
  }

let split n =
  { n = Nodes.empty;
    p = Place.elementary_split n;
    l = Link.id_empty;
  }

let one =
  { n = Nodes.empty;
    p = Place.one;
    l = Link.id_empty;
  }

let zero =
  { n = Nodes.empty;
    p = Place.zero;
    l = Link.id_empty;
  }

let sym (Inter (m, i)) (Inter (n, j)) =
  { n = Nodes.empty;
    p = Place.elementary_sym m n;
    l = Link.tens (Link.elementary_id i) (Link.elementary_id j) 0;
  }

let ion f c =
  { n = Nodes.add 0 c Nodes.empty;
    p = Place.elementary_ion;
    l = Link.elementary_ion f;
  }

let ion_chk f c =
  if (Ctrl.arity c) <> (Link.Face.cardinal f) then
    raise (CTRL_ERROR (Ctrl.arity c, f))
  else
    ion f c

let sub ~inner ~outer =
  { n = Nodes.empty;
    p = Place.id0;
    l = Link.elementary_sub ~inner ~outer;
  }

let closure inner = sub ~inner ~outer:Link.Face.empty

let intro outer = sub ~inner:Link.Face.empty ~outer

(* (l list of parents for each site) r roots *)
let placing l r f =
  { n = Nodes.empty;
    p = Place.parse_placing l r;
    l = Link.elementary_id f;
  }

(* Empty link graph and no nodes in the place graph. *)
let is_plc b =
  (Link.Lg.equal b.l Link.id_empty) &&
  (Nodes.size b.n = 0) &&
  (Place.is_plc b.p)

(* Empty place graph and no nodes in the link graph. *)
let is_wir b =
  (b.p = Place.id0) && (Nodes.size b.n = 0)

let is_id b =
  (Nodes.is_empty b.n) && (Place.is_id b.p) && (Link.is_id b.l)

let tens a b =
  { n = Nodes.tens a.n b.n;
    p = Place.tens a.p b.p;
    l = Link.tens a.l b.l (Nodes.size a.n);
  }

let tens_of_list =
  List.fold_left (fun acc b -> tens acc b) id_eps

let rec fold i n op f res =
  if i < n then fold (i + 1) n op f (op res (f i))
  else res

let tens_seq ~start ~stop f =
  fold start stop tens f id_eps

let comp a b =
  try { n = Nodes.tens a.n b.n;
        p = Place.comp a.p b.p;
        l = Link.comp a.l b.l (Nodes.size a.n);
      }
  with
  | Place.COMP_ERROR _
  | Link.FACES_MISMATCH _ ->
    raise (COMP_ERROR (inner a, outer b))

let comp_of_list =
  List.fold_left (fun acc b -> comp acc b) id_eps

let comp_seq ~start ~stop f =
  fold start stop comp f id_eps

let ppar a b =
  { n = Nodes.tens a.n b.n;
    p = Place.tens a.p b.p;
    l = Link.ppar a.l b.l (Nodes.size a.n);
  }

let ppar_of_list =
  List.fold_left (fun acc b -> ppar acc b) id_eps

let ppar_seq ~start ~stop f =
  fold start stop ppar f id_eps

let par a b =
  let p = ppar a b in
  let out_p = outer p in
  comp (tens (merge (ord_of_inter out_p))
          (id (Inter (0, face_of_inter out_p))))
    p

let par_of_list =
  List.fold_left (fun acc b -> par acc b) id_eps

let par_seq ~start ~stop f =
  fold start stop par f id_eps

let nest a b =
  let idx = id (Inter (0, face_of_inter (outer b))) in
  comp (ppar a idx) b

(* share f by psi in g check that psi is a placing *)
let share f psi g =
  if is_plc psi
  then comp (comp g (tens psi (id (Inter (0, face_of_inter (outer f)))))) f
  else raise SHARING_ERROR

(* not checking if f is a subset of b's outer names *)
let close f b =
  let g = Link.Face.diff (face_of_inter (outer b)) f
  and n = ord_of_inter (outer b)
  and cs = 
    Link.Face.elements f
    |> List.map (fun n ->
        closure (Link.Face.singleton n))
    |> tens_of_list in
  comp (tens cs (id (Inter (n, g)))) b

(* renaming through subsitution (o/i * b) *)
let rename ~inner:i ~outer:o b =
  let g = Link.Face.diff (face_of_inter (outer b)) i
  and n = ord_of_inter (outer b)
  and s = sub ~inner:i ~outer:o in
  comp (tens s (id (Inter (n, g)))) b

let atom f c =
  comp (ion f c) one

let atom_chk f c =
  comp (ion_chk f c) one

let is_mono b =
  (Place.is_mono b.p) && (Link.is_mono b.l)

let is_epi b =
  (Place.is_epi b.p) && (Link.is_epi b.l)

let is_guard b =
  (Place.is_guard b.p) && (Link.is_guard b.l)

let is_solid b =
  (is_epi b) && (is_mono b) && (is_guard b)

let is_ground b =
  (Place.is_ground b.p) && (Link.is_ground b.l)

(* TO DO *)
(*let latex_of_big = function | Bg (ns, p, l) -> "latex representation"*)

let apply i b =
  { n = Nodes.apply i b.n;
    p = Place.apply i b.p;
    l = Link.apply i b.l;
  }

let to_dot b ide =
  let build_rank i flag =
    let ord = ord_of_inter i
    and f = face_of_inter i in
    if (ord = 0) && (Link.Face.is_empty f) then ""
    else if flag then
      let ss =
        (List.map (fun i ->
             sprintf "r%d" i) (IntSet.elements (IntSet.of_int ord))) @
        (List.map (fun (Link.Name n) ->
             sprintf "\"o%s\"" n) (Link.Face.elements f)) in
      match ss with
      | [] -> ""
      | _ -> sprintf "{ rank=source; %s };\n" (String.concat "; " ss)
    else
      let xs =
        (List.map (fun i ->
             sprintf "s%d" i) (IntSet.elements (IntSet.of_int ord))) @
        (List.map (fun (Link.Name n) ->
             sprintf "\"i%s\"" n) (Link.Face.elements f)) in
      match xs with
      | [] -> ""
      | _ -> sprintf "{ rank=sink; %s };\n" (String.concat "; " xs) in
  let (inner_shp, outer_shp, hyp_shp, link_adj) = Link.get_dot b.l
  and (roots_shp, sites_shp, node_ranks, place_adj) = Place.get_dot b.p
  and nodes_shp = Nodes.to_dot b.n
  and rank_out = build_rank (outer b) true
  and rank_in = build_rank (inner b) false in
  sprintf "digraph \"%s\" {\n\
           stylesheet = \"style.css\"\n\
           %s%s%s%s%s\n\
           %s%s%s%s%s%s}"
    ide roots_shp outer_shp rank_out hyp_shp nodes_shp sites_shp
    node_ranks inner_shp rank_in place_adj link_adj

let decomp ~target ~pattern ~i_n ~i_e f_e =
  let (p_c, p_d, p_id, i_c, i_d) =
    Place.decomp ~target:target.p ~pattern:pattern.p i_n in
  let (l_c, l_d, l_id) =
    Link.decomp ~target:target.l ~pattern:pattern.l ~i_e ~i_c ~i_d f_e
  and (n_c, n_d) =
    (Nodes.apply i_c target.n, Nodes.apply i_d target.n) in
  ({ p = p_c; l = l_c; n = n_c },
   { p = p_d; l = l_d; n = n_d },
   { p = p_id; l = l_id; n = Nodes.empty })

(* Generates an iso from a matrix of assignments *)
let get_iso solver m =
  snd (Array.fold_left (fun (i, iso) r ->
      (i + 1, snd (Array.fold_left (fun (j, iso) x ->
           match solver#value_of x with
           | Minisat.True -> (j + 1, Iso.add i j iso)
           | Minisat.False | Minisat.Unknown -> (j + 1, iso)
         ) (0, iso) r))
    ) (0, Iso.empty) m)

let get_fun solver m =
  snd (Array.fold_left (fun (i, f) r ->
      (i + 1, snd (Array.fold_left (fun (j, f) x ->
           match solver#value_of x with
           | Minisat.True -> (j + 1, Fun.add i j f)
           | Minisat.False | Minisat.Unknown -> (j + 1, f)
         ) (0, f) r))
    ) (0, Fun.empty) m)

(************************** DEBUG *************************)
(* let string_of_SAT solver m = *)
(*   let r = Array.length m *)
(*   and c = try Array.length m.(0) with _ -> 0 *)
(*   and iso = Iso.to_string (get_iso solver m) in *)
(*   sprintf "%d X %d : %s" r c iso *)

type sat_vars = {
  iso_nodes : Minisat.var array array;
  z0_rows : Minisat.var array array;
  z0_cols : Minisat.var array array;
  iso_edges : Minisat.var array array;
  map_edges_r : Iso.t;
  map_edges_c : Iso.t;
  z1_rows : Minisat.var array array;
  z1_cols : Minisat.var array array;
  z2 : Minisat.var array array;
  iso_hyp : Minisat.var array array;
  map_hyp_r : Iso.t;
  map_hyp_c : Iso.t;
  z3 : Minisat.var array array;
}

(* let print_dump solver v = *)
(*   printf "------ ISO NODES\n\ *)
     (*           %s\n\ *)
     (*           ------ Z0 ROWS\n\ *)
     (*           %s\n\ *)
     (*           ------ Z0 COLUMNS\n\ *)
     (*           %s\n\ *)
     (*           ------ ISO EDGES\n\ *)
     (*           %s\n\ *)
     (*           ------ Z1 ROWS\n\ *)
     (*           %s\n\ *)
     (*           ------ Z1 COLUMNS\n\ *)
     (*           %s\n\ *)
     (*           ------ Z2\n\ *)
     (*           %s\n\ *)
     (*           ------ ISO HYPER\n\ *)
     (*           %s\n\ *)
     (*           ------ Z3\n\ *)
     (*           %s\n" *)
(*     (string_of_SAT solver v.iso_nodes)  *)
(*     (string_of_SAT solver v.z0_rows) *)
(*     (string_of_SAT solver v.z0_cols)  *)
(*     (string_of_SAT solver v.iso_edges) *)
(*     (string_of_SAT solver v.z1_rows) *)
(*     (string_of_SAT solver v.z1_cols) *)
(*     (string_of_SAT solver v.z2) *)
(*     (string_of_SAT solver v.iso_hyp) *)
(*     (string_of_SAT solver v.z3) *)

(**********************************************************)

let add_blocking solver v w =
  let scan_matrix m =
    Array.fold_left (fun (i, acc) r ->
        (i + 1,
         Array.fold_left (fun (j, acc) x ->
             match solver#value_of x with
             | Minisat.True -> (j + i, neg_lit x :: acc)
             (* Check if this is really necessary *)
             | Minisat.False -> (j + 1, pos_lit x :: acc)
             | Minisat.Unknown -> assert false) (*BISECT-IGNORE*)
           (0, acc) r)
        |> snd)
      (0, []) m
    |> snd in
  solver#add_clause ((scan_matrix v) @ (scan_matrix w))

(* let print_solver_matrix solver v = *)
(*   Array.iter (fun i -> *)
(*       Array.iter (fun j -> *)
(*           match solver#value_of j with *)
(*           | Minisat.True -> print_string "t" *)
(*           | Minisat.False -> print_string "f" *)
(*           | Minisat.Unknown -> print_string "-") *)
(*         i; *)
(*       print_newline ();) *)
(*     v *)

let rec filter_loop solver t p vars t_trans =
  solver#simplify;
  match solver#solve with
  | Minisat.UNSAT -> raise_notrace NO_MATCH
  | Minisat.SAT ->
    begin
      let iso_v = get_iso solver vars.iso_nodes
      (* and iso_e = get_iso solver w e f *) in
      if (Place.check_match ~target:t.p ~pattern:p.p t_trans iso_v) then
        (solver, vars)
      else begin
        (* eprintf "Warning: invalid match not discarded by SAT. \ *)
               (*          Removing it ...\n"; *)
        add_blocking solver vars.iso_nodes vars.iso_edges;
        filter_loop solver t p vars t_trans
      end
    end

let add_c4 t p n_t n_p solver v =
  let (t_constraints, exc_clauses, js) =
    Place.match_list ~target:t ~pattern:p ~n_t ~n_p in
  Cnf.post_conj_m exc_clauses solver v;
  (Array.of_list
     (List.fold_left (fun acc x ->
          (Cnf.post_tseitin x solver v) :: acc)
         [] t_constraints),
   js)

let add_c5 target pattern n_t n_p solver v =
  let (clauses_l, js_l) =
    Place.match_leaves ~target ~pattern ~n_t ~n_p
  and (clauses_o, js_o) =
    Place.match_orphans ~target ~pattern ~n_t ~n_p in
  Cnf.post_conj_m (clauses_l @ clauses_o) solver v;
  IntSet.union js_l js_o

let add_c6 target pattern n_t n_p solver v =
  let (clauses_s, js_s) =
    Place.match_sites ~target ~pattern ~n_t ~n_p
  and (clauses_r, js_r) =
    Place.match_regions ~target ~pattern ~n_t ~n_p in
  Cnf.post_conj_m (clauses_s @ clauses_r) solver v;
  IntSet.union js_s js_r

(* Each row of the input matrix aux is the commander-variable encoding of the
   corresponding column of the iso matrix m. *)
let add_c11 unmatch_v solver m (aux : Minisat.var array array) rc_v =
  match rc_v with
  | [] -> (
      (* No commander-variable encoding *)
      IntSet.iter (fun j ->
          Cnf.post_block j solver m
        ) unmatch_v
    )
  | _ -> (
      IntSet.iter (fun i ->
          Cnf.post_block_cmd i solver aux rc_v
        ) unmatch_v
    )

(* let print_clauses x = *)
(*   List.map Cnf.string_of_clause x *)
(*   |> String.concat "; " *)
(*   |> (fun x -> *)
(*       print_endline ("[" ^ x ^ "]")) *)

let add_c7 target pattern n_t n_p aux rc_w solver w =
  let (clauses, b_cols, b_pairs) =
    Link.match_edges ~target ~pattern ~n_t ~n_p in
  Cnf.post_conj_m (clauses @ b_pairs) solver w;
  add_c11 b_cols solver w aux rc_w;
  clauses

let add_c8 target pattern n_t n_p clauses solver v w =
  let constraints =
    Link.match_ports ~target ~pattern ~n_t ~n_p clauses in
  List.iter (fun x ->
      Cnf.post_impl x solver w v)
    constraints

let add_c10 target pattern solver v =
  Cnf.post_conj_m (Place.match_trans ~target ~pattern) solver v

(* Cnf.tot does not introduce commander-variables on columns. Using
   post_block. *)
let add_c9 target pattern n_t n_p solver v =
  let (r, c, constraints, blocks, blocks_f, iso_p, iso_open) =
    Link.match_peers ~target ~pattern ~n_t ~n_p in
  let w = Cnf.init_aux_m r c solver in
  List.iter (fun x ->
      Cnf.post_impl x solver w v)
    constraints;
  Cnf.post_conj_m blocks_f solver w;
  let (aux, _) =
    Cnf.post_tot (Cnf.tot_fun r c 6 3) solver w in
  Cnf.post_conj_m (Cnf.blocking_pairs blocks) solver w;
  (w, aux, iso_p, iso_open)

(* Block columns in W' when the corresponding column in W is in a match *)
let add_c12 solver w iso_w w' iso_w' aux_bij_w_cols rc_w =
  (* T index -> W' index *)
  let inv_w' = Iso.inverse iso_w' in
  let convert_j j =
    safe (Iso.apply inv_w' (safe (Iso.apply iso_w j)))
  and vars_of_col j m =
    snd (
      Array.fold_left (fun (i, acc) _ ->
          (i + 1, (Cnf.N_var (Cnf.M_lit (i, j))) :: acc)
        ) (0, []) m
    ) in
  let cols_w = Iso.dom iso_w in
  match rc_w with
  | [] -> (* no commander-variable encoding on the columns *)
    List.iter (fun j ->
        let vars_w = vars_of_col j w
        and vars_w' = vars_of_col (convert_j j) w' in
        Cnf.post_impl2 vars_w vars_w' solver w w'
      ) cols_w
  | _ ->
    List.iter (fun j ->
        let vars_w =
          List.map (fun r ->
              Cnf.N_var (Cnf.M_lit (j, r))
            ) rc_w
        and vars_w' = vars_of_col (convert_j j) w' in
        Cnf.post_impl2 vars_w vars_w' solver aux_bij_w_cols w'
      ) cols_w

(* Compute isos from nodes in the pattern to nodes in the target *)
let aux_match t p t_trans =
  try
    let solver = new solver
    and (n, m) = (p.p.Place.n, t.p.Place.n)
    and (closed_p, iso_w_r) = Link.closed_edges_iso p.l
    and (closed_t, iso_w_c) = Link.closed_edges_iso t.l in
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
    (* Add C6: sites and regions in the place graphs. *)
    and js2 = add_c6 t.p p.p t.n p.n solver v in
    (* Add C7: edges in the pattern are matched to edges in the target. *)
    let clauses =
      add_c7 closed_t closed_p t.n p.n aux_bij_w_cols rc_w solver w in
    (* Add C8: ports of matched closed edges have to be isomorphic. *)
    add_c8 closed_t closed_p t.n p.n clauses solver v w;
    (* Add C9: ports of matched open edges have to be isomorphic.
       Return matrix from open edges in the pattern to non-empty edges in the
       target. *)
    let (w', aux_bij_w'_rows, iso_w'_r, iso_w'_c) =
      add_c9 t.l p.l t.n p.n solver v in
    (* Add C10: block edges between unconnected nodes with sites and nodes with
       regions. *)
    add_c10 t.p p.p solver v;
    (* If an edge is in a match in w then forbid matches in w' *)
    add_c12 solver w iso_w_c w' iso_w'_c aux_bij_w_cols rc_w;
    (* Block unmatchable columns *)
    let unmatch_v =
      IntSet.diff
        (IntSet.of_int m)
        (IntSet.union js0 (IntSet.union js1 js2)) in
    add_c11 unmatch_v solver v aux_bij_v_cols rc_v;
    let vars = {
      iso_nodes = v;
      z0_rows = aux_bij_v_rows;
      z0_cols = aux_bij_v_cols;
      iso_edges = w;
      map_edges_r = iso_w_r;
      map_edges_c = iso_w_c;
      z1_rows = aux_bij_w_rows;
      z1_cols = aux_bij_w_cols;
      z2 = zs4;
      iso_hyp = w';
      map_hyp_r = iso_w'_r;
      map_hyp_c = iso_w'_c;
      z3 = aux_bij_w'_rows;
    } in
    filter_loop solver t p vars t_trans
  with
  | Place.NOT_TOTAL -> raise_notrace NO_MATCH
  | Link.NOT_TOTAL -> raise_notrace NO_MATCH

(* true when p is not a match *)
let quick_unsat t p =
  ((Nodes.size p.n) > (Nodes.size t.n))
  || (Sparse.entries p.p.Place.nn > Sparse.entries t.p.Place.nn)
  || (Nodes.not_sub p.n t.n)
  || (IntSet.cardinal (Place.leaves p.p) > IntSet.cardinal (Place.leaves t.p))
  || (IntSet.cardinal (Place.orphans p.p) > IntSet.cardinal (Place.orphans t.p))
  || (Link.closed_edges p.l > Link.closed_edges t.l)
  || (Link.max_ports p.l > Link.max_ports t.l)

let occurs ~target:t ~pattern:p =
  try
    if Nodes.size p.n = 0 then true
    else begin
      if quick_unsat t p then false
      else begin
        let t_trans = Place.trans t.p in
        ignore (aux_match t p t_trans);
        true
      end
    end
  with
  | NO_MATCH -> false

let occurrence ~target:t ~pattern:p t_trans =
  (* replace with an assertion *)
  if Nodes.size p.n = 0 then raise NODE_FREE
  else if quick_unsat t p then None
  else
    begin try
        let (s, vars) = aux_match t p t_trans in
        let i_v = get_iso s vars.iso_nodes
        and i_e =
          Iso.transform
            ~iso_dom:vars.map_edges_r
            ~iso_codom:vars.map_edges_c
            (get_iso s vars.iso_edges)
        and i_h =
          Fun.transform
            ~iso_dom:vars.map_hyp_r
            ~iso_codom:vars.map_hyp_c
            (get_fun s vars.iso_hyp) in
        Some (i_v, i_e, i_h)
      with
      | NO_MATCH -> None
    end

(* compute non-trivial automorphisms of b *)
let auto b =
  if Nodes.size b.n = 0 then raise NODE_FREE
  else begin
    let b_trans = Place.trans b.p
    and rem_id res =
      List.filter (fun (i, e) ->
          not ((Iso.is_id i) && (Iso.is_id e))
        ) res in
    rem_id (try
              let (s, vars) = aux_match b b b_trans in
              let rec loop_occur res =
                add_blocking s vars.iso_nodes vars.iso_edges;
                try
                  ignore (filter_loop s b b vars b_trans);
                  loop_occur (
                    ((get_iso s vars.iso_nodes),
                     (get_iso s vars.iso_edges) (* matrix indices *)
                    ) :: res)
                with
                | NO_MATCH -> res in
              loop_occur [(get_iso s vars.iso_nodes,
                           get_iso s vars.iso_edges)] (* matrix indices *)
            with
            | NO_MATCH -> [])
  end

let clause_of_iso iso m =
  snd
    (Array.fold_left (fun (i, acc) r ->
         (i + 1, snd
            (Array.fold_left (fun (j, acc) x ->
                 if safe (Iso.apply iso i) = j then
                   (j + 1, neg_lit x :: acc)
                 else (j + 1, pos_lit x :: acc)) (* Do we really need this? *)
                (0, acc) r)))
        (0, []) m)

let occurrences ~target:t ~pattern:p =
  if Nodes.size p.n = 0 then raise NODE_FREE
  else if quick_unsat t p then []
  else begin
    try
      (************************** DEBUG *************************)
      (* printf "------- TARGET:\n%!\ *)
             (*         %s\n\ *)
             (*         ------- PATTERN:\n%!\ *)
             (*         %s\n" (to_string t) (to_string p); *)
      (**********************************************************)
      let t_trans = Place.trans t.p in
      let (s, vars) = aux_match t p t_trans in
      let autos = auto p in
      let rec loop_occur res =
        add_blocking s vars.iso_nodes vars.iso_edges;
        (****************AUTOMORPHISMS****************)
        let gen =
          List.combine
            (Iso.gen_isos
               (get_iso s vars.iso_nodes) (List.map fst autos))
            (Iso.gen_isos
               (get_iso s vars.iso_edges) (List.map snd autos)) in
        List.iter (fun (iso_i, iso_e) ->
            s#add_clause (
              (clause_of_iso iso_i vars.iso_nodes) @
              (clause_of_iso iso_e vars.iso_edges)
            )
          ) gen;
        (*********************************************)
        try
          ignore (filter_loop s t p vars t_trans);
          loop_occur
            ((get_iso s vars.iso_nodes,
              Iso.transform
                ~iso_dom:vars.map_edges_r
                ~iso_codom:vars.map_edges_c
                (get_iso s vars.iso_edges),
              Fun.transform
                ~iso_dom:vars.map_hyp_r
                ~iso_codom:vars.map_hyp_c
                (get_fun s vars.iso_hyp))
             :: res)
        with
        | NO_MATCH -> res in
      loop_occur
        [ (get_iso s vars.iso_nodes,
           Iso.transform
             ~iso_dom:vars.map_edges_r
             ~iso_codom:vars.map_edges_c
             (get_iso s vars.iso_edges),
           Fun.transform
             ~iso_dom:vars.map_hyp_r
             ~iso_codom:vars.map_hyp_c
             (get_fun s vars.iso_hyp)) ]
    with
    | NO_MATCH -> []
  end

let equal_SAT a b =
  (************************** DEBUG *************************)
  (* printf "------- A:\n%!\ *)
     (*         %s\n\ *)
     (*         ------- B:\n%!\ *)
     (*         %s\n" (to_string a) (to_string b); *)
  (**********************************************************)
  try
    let solver = new solver in
    let n = a.p.Place.n
    and h = Link.Lg.cardinal a.l in
    let v_n = Cnf.init_aux_m n n solver
    and v_l = Cnf.init_aux_m h h solver in
    Cnf.post_one_to_one (Cnf.one_to_one n 6 3) solver v_n;
    Cnf.post_one_to_one (Cnf.one_to_one h 6 3) solver v_l;
    (* Place graph *)
    let (t_constraints, exc_clauses, _) =
      Place.match_list_eq a.p b.p a.n b.n
    and (c_rn, _) =
      Place.match_region_nodes a.p b.p a.n b.n
    and (c_ns, _) =
      Place.match_nodes_sites a.p b.p a.n b.n
    and (clauses_l, _) =
      Place.match_leaves ~target:b.p ~pattern:a.p ~n_t:b.n ~n_p:a.n
    and (clauses_o, _) =
      Place.match_orphans ~target:b.p ~pattern:a.p ~n_t:b.n ~n_p:a.n in
    (* let cols = *)
    (*   IntSet.union *)
    (*     (IntSet.union js_o js_l) *)
    (*     (IntSet.union cols0 (IntSet.union cols1 cols2)) in *)
    (* if IntSet.cardinal cols <> n then raise NO_MATCH; *)
    Cnf.post_conj_m exc_clauses solver v_n;
    Cnf.post_conj_m (c_rn @ c_ns) solver v_n;
    Cnf.post_conj_m (clauses_l @ clauses_o) solver v_n;
    List.iter (fun x ->
        ignore (Cnf.post_tseitin x solver v_n)
      ) t_constraints;
    (* Link graph *)
    let (clauses, b_pairs) =
      Link.match_list_eq a.l b.l a.n b.n in
    Cnf.post_conj_m (clauses @ b_pairs) solver v_l;
    let l_constraints =
      Link.match_ports_eq a.l b.l a.n b.n clauses in
    List.iter (fun x ->
        Cnf.post_impl x solver v_l v_n
      ) l_constraints;
    solver#simplify;
    match solver#solve with
    | Minisat.UNSAT -> false
    | Minisat.SAT -> true
  with
  | Place.NOT_TOTAL -> false
  | Link.NOT_TOTAL -> false
  | NO_MATCH -> false

type big_key = int

let key b =
  Hashtbl.hash
    (b.p.Place.r,
     b.p.Place.s,
     Place.size b.p,
     IntSet.cardinal (Place.leaves b.p),
     IntSet.cardinal (Place.orphans b.p),
     Link.Lg.cardinal b.l,
     Link.closed_edges b.l,
     Link.string_of_face (Link.inner b.l),
     Link.string_of_face (Link.outer b.l),
     String.concat "~" (Nodes.norm b.n))

(* Comparison over keys already performed and failed *)
let equal_opt a b =
  (Place.deg_regions a.p = Place.deg_regions b.p)
  && (Place.deg_sites a.p = Place.deg_sites b.p)
  && (Sparse.equal a.p.Place.rs b.p.Place.rs)
  && (* Placing or wiring *)
  if Nodes.size b.n = 0 then
    (Place.equal_placing a.p b.p) && (Link.Lg.equal a.l b.l)
  else
    equal_SAT a b

let equal a b =
  (Link.Lg.cardinal a.l = Link.Lg.cardinal b.l)
  && (inter_equal (inner a) (inner b))
  && (inter_equal (outer a) (outer b))
  && (Place.size a.p = Place.size b.p)
  && (Place.deg_regions a.p = Place.deg_regions b.p)
  && (Place.deg_sites a.p = Place.deg_sites b.p)
  && (Nodes.equal a.n b.n)
  && (Sparse.equal a.p.Place.rs b.p.Place.rs)
  && (* Placing or wiring *)
  if Nodes.size b.n = 0 then
    (Place.equal_placing a.p b.p) && (Link.Lg.equal a.l b.l)
  else
    equal_SAT a b

let prime_components b =
  let (pgs, isos) =
    List.split (Place.prime_components b.p) in
  let lgs = Link.prime_components b.l isos in
  List.map (fun ((p, l), iso) ->
      { p = p;
        l = l;
        n = Nodes.apply iso b.n;
      })
    (List.combine (List.combine pgs lgs) isos)

let instantiate eta b =
  let bs = prime_components b in
  Fun.fold (fun _ s acc ->
      try ppar acc (List.nth bs s) with
      | Failure _ | Invalid_argument _ ->                     (*BISECT-IGNORE*)
        assert false (* eta is assumed total *)) (*BISECT-IGNORE*)
    eta id_eps

(* Decomposition of argument D = D' x D_id *)
let decomp_d d id =
  let (p_d, p_id, iso_d, iso_id) = Place.decomp_d d.p id in
  let lgs = Link.prime_components d.l [iso_d; iso_id] in
  match lgs with
  | [l_d; l_id] ->
    ({ p = p_d;
       l = l_d;
       n = Nodes.apply iso_d d.n;
     },
     { p = p_id;
       l = l_id;
       n = Nodes.apply iso_id d.n;
     })
  | _ -> assert false (*BISECT-IGNORE*)

let rewrite (i_n, i_e, f_e) ~s ~r0 ~r1 eta =
  let (c, d, id) = decomp ~target:s ~pattern:r0 ~i_n ~i_e f_e in
  match eta with
  | None -> comp c (comp (tens r1 id) d)
  | Some eta' ->
    if Fun.is_id eta' then
      comp c (comp (tens r1 id) d)
    else  
    (* Normalise link graph *)
    let (omega_l, d_norm_l) = Link.norm d.l in
    let d_norm =
      { d with l = d_norm_l; } in
    let (d', d_id) = decomp_d d_norm (ord_of_inter (inner id)) in
    let d'' = ppar (instantiate eta' d') d_id in
    let omega = { p = Place.elementary_id (d''.p.Place.r);
                  l = omega_l;
                  n = Nodes.empty; } in
    comp omega d'' 
    |> comp (tens r1 id)
    |> comp c
