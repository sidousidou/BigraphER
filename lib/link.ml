open Base
open Printf

(* ide strings no capital letter or number at the start *)
type name = Nam of string

module Face = Set.Make (
  struct
    type t = name
    let compare = fun (Nam s0) (Nam s1) -> 
      String.compare s0 s1
  end)

(* Module used to compute equivalence classes *)
module Face_set = Set.Make (
  struct
    type t = Face.t
    let compare = Face.compare
  end)

(* (in, out, ports) *)
type edg = { i: Face.t; o: Face.t; p: Ports.t }

let edg_compare (h : edg) (k : edg) =
  match Face.compare h.i k.i with
  | 0 -> begin
      match Face.compare h.o k.o with
      | 0 -> Ports.compare h.p k.p
      | x -> x
    end
  | x -> x  

module Lg = Set.Make (struct
    type t = edg
    let compare = edg_compare
  end)

(* tensor product fails (inner common names, outer common names)*)
exception NAMES_ALREADY_DEFINED of (Face.t * Face.t)

(* Composition fails *)
exception FACES_MISMATCH of (Face.t * Face.t)

let string_of_name (Nam s) = s

let parse_face =
  List.fold_left (fun acc x -> 
      Face.add (Nam x) acc) Face.empty

let string_of_face f =
  sprintf "{%s}"
    (String.concat ", " (List.map string_of_name (Face.elements f))) 

let string_of_edge e =
  sprintf "(%s, %s, %s)" 
    (string_of_face e.i) (string_of_face e.o) (Ports.to_string e.p)

let to_string l = 
  String.concat "\n" (List.map string_of_edge (Lg.elements l))

(* Nodes are counted starting from 1 *)
let parse lines = 
  let build_edge s n h =
    let a  = Str.split (Str.regexp_string " ") s in 
    { p =
	List.fold_left (fun acc x ->
	    try
	      let j = Hashtbl.find h x in
	      Hashtbl.add h x (j + 1);
	      Ports.add (x, j) acc
	    with
	    | Not_found -> 
	      begin
		Hashtbl.add h x 1;
		Ports.add (x, 0) acc
	      end) Ports.empty (List.map (fun x ->
	    (int_of_string x) - 1) (List.tl (List.rev a)));
      i = Face.empty;
      o = begin
	match List.nth a ((List.length a) - 1) with 
	| "t" -> parse_face [sprintf "n%d" n]
	| _ -> Face.empty
      end;
    } in
  let h = Hashtbl.create (List.length lines) in
  (fst (List.fold_left (fun (acc, i) l -> 
       (Lg.add (build_edge l i h) acc), i + 1) (Lg.empty, 0) lines), h)

(* Elementary substitution: one edge without ports *)
let elementary_sub f_i f_o =
  if Face.is_empty f_i && Face.is_empty f_o then 
    Lg.empty
  else Lg.singleton { i = f_i; o = f_o; p = Ports.empty }

(* Node index is 0. Ports are from 0 to |f| - 1 *)
let elementary_ion f =
  fst (Face.fold (fun n (acc, i) ->
      let e = Lg.singleton { i = Face.empty;
			     o = Face.singleton n;
			     p = Ports.singleton (0, i);
			   } in
      (Lg.union e acc, i + 1)) f (Lg.empty, 0))

let inner (lg : Lg.t) =
  Lg.fold (fun e acc -> 
      Face.union e.i acc) lg Face.empty

let outer (lg : Lg.t) =
  Lg.fold (fun e acc -> 
      Face.union e.o acc) lg Face.empty

let ports lg = 
  Lg.fold (fun e acc -> 
      Ports.union e.p acc) lg Ports.empty

(* Add offset m to all the port indeces *)
let offset (lg : Lg.t) (m : int) =
  Lg.fold (fun e acc ->
      Lg.add { i = e.i; 
	       o = e.o;
	       p = Ports.fold (fun (i, p) acc ->
	           Ports.add (i + m, p) acc) e.p Ports.empty;
	     } acc) lg Lg.empty

(* n0 is necessary because some nodes my be present in the left place      *)
(* graph but not in the link graph.                                        *)
let tens lg0 lg1 n0 =
  let i_in = Face.inter (inner lg0) (inner lg1)
  and i_out = Face.inter (outer lg0) (outer lg1)
  in if Face.is_empty i_in && Face.is_empty i_out then
    Lg.union lg0 (offset lg1 n0)
  else raise (NAMES_ALREADY_DEFINED (i_in, i_out))

(* Identity. One edge for every name in f *)
let elementary_id f =
  Face.fold (fun n acc ->
      Lg.union (Lg.singleton { i = Face.singleton n; 
			       o = Face.singleton n; 
			       p = Ports.empty;
			     }) acc) f Lg.empty

let id_empty = elementary_id Face.empty

let is_id l =
  Lg.for_all (fun e ->
      Face.equal e.i e.o && (Face.cardinal e.i = 1) && Ports.is_empty e.p) l

(* Merge two sets of equivalence classes *)
let equiv_class a b =
  let u = Face_set.union a b in
  let rec fix_point s res =
    try
      ((* Smallest face in set s *)
	let f = Face_set.min_elt s in
	(* set s without face f *) 
	let new_s = Face_set.remove f s in
	try
          (* Largest face having names in common with f *) 
          let c = Face_set.max_elt
              (Face_set.filter (fun x ->
                   not (Face.is_empty (Face.inter x f))) new_s) in
          let new_c = Face.union c f in    
          fix_point (Face_set.add new_c (Face_set.remove c new_s)) res    
	with
        | _ -> fix_point new_s (Face_set.add f res))
    with
    | _ -> res    
  in fix_point u Face_set.empty

(* Merge a set of edges *)
let merge lg =
  { i = inner lg;
    o = outer lg;
    p = Lg.fold (fun e acc -> Ports.union e.p acc) lg Ports.empty;
  }

(* Merge a set of edges on the inner face according to equivalence classes *)
let merge_in lg cls =
  Face_set.fold (fun f acc ->
      Lg.add (merge (Lg.filter (fun e ->
          not (Face.is_empty (Face.inter f e.i))) lg)) acc) cls Lg.empty   

(* Merge a set of edges on the outer face according to equivalence classes *)       
let merge_out lg cls =
  Face_set.fold (fun f acc ->
      Lg.add (merge (Lg.filter (fun e ->
          not (Face.is_empty (Face.inter f e.o))) lg)) acc) cls Lg.empty   

(* Fuse two link graphs on common names *)
let fuse a b = 
  Lg.fold (fun e acc ->
      let h =
        Lg.choose (Lg.filter (fun h -> Face.equal h.o e.i) b) in
      let new_e = {i = h.i; o = e.o; p = Ports.union e.p h.p} in
      if Face.is_empty new_e.i && Face.is_empty new_e.o &&
         Ports.is_empty new_e.p then acc
      else Lg.add new_e acc) a Lg.empty

(* Composition A o B. [n] is the number of nodes in A. *)
let comp a b n = 
  let x = inner a
  and y = outer b in
  if Face.equal x y 
  then 
    (let new_b = offset b n 
     and cls_in = Lg.fold (fun e acc ->
         Face_set.add e.i acc) a Face_set.empty in
     let cls_out = Lg.fold (fun e acc -> 
         Face_set.add e.o acc) new_b Face_set.empty in
     let cls = equiv_class cls_in cls_out in
     Lg.union (fuse (merge_in a cls) (merge_out new_b cls))
       (Lg.union 
	  (Lg.filter (fun e -> Face.is_empty e.i) a) 
	  (Lg.filter (fun e -> Face.is_empty e.o) new_b)))    
  else raise (FACES_MISMATCH (x, y))

(* no inner names that are siblings *)
let is_mono l =
  Lg.for_all (fun e -> Face.cardinal e.i < 2) l

(* no idle outer names *)
let is_epi l =
  Lg.for_all (fun e ->
      if Face.cardinal e.o > 0 then (Face.cardinal e.i > 0 || Ports.cardinal e.p > 0)
      else true) l

let is_guard l =
  (* true if inner are connected to outer *)
  not (Lg.exists (fun e ->
      (not (Face.is_empty e.i)) && (not (Face.is_empty e.o))) l)

(* Rename names in f_i and f_o *)
let rename_shared l i f_i f_o =
  let rename_face f i shr =
    Face.fold (fun n acc ->
        if Face.mem n shr then
          Face.add (Nam (sprintf "%d%s" i (string_of_name n))) acc
        else
          Face.add n acc) f Face.empty in 
  Lg.fold (fun e acc ->
      let new_e =
        {i = rename_face e.i i f_i;
         o = rename_face e.o i f_o;
         p = e.p
        } in Lg.add new_e acc) l Lg.empty

(* Duplicate the names in a face and build the substitutions *)
let dup_out f =
  Face.fold (fun n acc ->
      tens
        (elementary_sub
           (Face.add (Nam (sprintf "0%s" (string_of_name n)))
              (Face.singleton (Nam (sprintf "1%s" (string_of_name n)))))
           (Face.singleton n))
        acc 0) f Lg.empty    

let dup_in f =
  Face.fold (fun n acc ->
      tens
        (elementary_sub
           (Face.singleton n)
           (Face.add (Nam (sprintf "0%s" (string_of_name n)))
              (Face.singleton (Nam (sprintf "1%s" (string_of_name n))))))
        acc 0) f Lg.empty  

(* Parallel product *)
let ppar a b n = 
  let shared_out = Face.inter (outer a) (outer b)
  and shared_in = Face.inter (inner a) (inner b) in
  let new_a = rename_shared a 0 shared_in shared_out
  and new_b = rename_shared b 1 shared_in shared_out in
  let a_b = tens new_a new_b n 
  and f_out = Face.union (outer a) (outer b)
  and f_in = Face.union (inner a) (inner b) in
  let wiring_out =
    tens (elementary_id (Face.diff f_out shared_out)) (dup_out shared_out) 0 in
  let wiring_in = 
    tens (elementary_id (Face.diff f_in shared_in)) (dup_in shared_in) 0 in
  comp wiring_out (comp a_b wiring_in n) 0

let apply_iso i l =
  Lg.fold (fun e acc ->
      Lg.add { i = e.i; o= e.o; p = Ports.apply e.p i } acc) l Lg.empty

(* Is e a hyperedge? An extra node is not required when it is an edge or an
   idle name.*)   
let is_hyp e = 
  (* closure on a port *)
  ((Ports.cardinal e.p) = 1 && (Face.is_empty e.i) && (Face.is_empty e.o)) ||
  (* idle alias on two names *)
  ((Face.is_empty e.i) && (Ports.is_empty e.p) && (Face.cardinal e.o = 2)) ||  
  (* closure on two names *)
  ((Face.is_empty e.o) && (Ports.is_empty e.p) && (Face.cardinal e.i = 2)) ||  
  (* more than 2 ports or names *)
  ((Ports.cardinal e.p) + (Face.cardinal e.i) + (Face.cardinal e.o) > 2)

(* is e an idle name? *)
let is_idle e = 
  (* inner name *)
  ((Face.is_empty e.o) && (Ports.is_empty e.p) && (Face.cardinal e.i = 1)) ||
  (* outer name *)
  ((Face.is_empty e.i) && (Ports.is_empty e.p) && (Face.cardinal e.o = 1))

let is_closed e =
  (Face.is_empty e.i) && (Face.is_empty e.o)

let get_dot l =
  match
    Lg.fold (fun e (i, buff_i, buff_o, buff_h, buff_adj) ->
        let flag = is_hyp e in
        ((* Edge index *)
	  i + 1,
          (* Inner names *)
          Face.fold (fun n buff ->
	      sprintf "%si%s [ shape=plaintext, label=\"%s\", width=.18,\
                       height=.18, fontname=\"serif\", fontsize=9.0 ];\n"
                buff (string_of_name n) (string_of_name n)) e.i buff_i,
          (* Outer names *)  
          Face.fold (fun n buff ->
	      sprintf "%so%s [ shape=plaintext, label=\"%s\", width=.18,\
                       height=.18, fontname=\"serif\", fontsize=9.0 ];\n"
                buff (string_of_name n) (string_of_name n)) e.o buff_o,
          (* Hyperedges *)   
          (if flag then sprintf
	       "%se%d [ shape=point, label=\"\", width=.0, height=.0, style=invis, color=green ];\n"
	       buff_h i    
	   else buff_h),
          (* Adjacency *)
          (if flag then
	     (buff_adj ^
              (Face.fold (fun n buff ->
		   sprintf "%si%s -> e%d;\n" buff (string_of_name n) i) e.i "") ^ 
              (Face.fold (fun n buff ->
		   sprintf "%so%s -> e%d;\n" buff (string_of_name n) i) e.o "") ^
              (if (Ports.cardinal e.p) = 1 && (Face.is_empty e.i) &&
		  (Face.is_empty e.o) then
		 (* closure from a port *)
		 sprintf "e%d -> v%d [ dir=both, arrowhead=dot, arrowtail=tee, weight=5 ];\n"
		   i (fst (Ports.choose e.p))
               else Ports.fold (fun (v, _) buff ->
		   sprintf "%se%d -> v%d [dir=both, arrowhead=dot, arrowtail=none ];\n" buff i v) e.p ""))  
	   else
             (* idle name *)
           if is_idle e then buff_adj
	   else   
             (* edge between two points *)
             (if Ports.is_empty e.p then
		(* name -> name *)
		sprintf "%si%s -> o%s;\n" buff_adj (string_of_name (Face.choose e.i))
		  (string_of_name (Face.choose e.o))
              else if Face.is_empty e.o then
		if Face.is_empty e.i then
		  (* port -> port *)
		  sprintf "%sv%d -> v%d [ dir=both, arrowtail=dot, arrowhead=dot ];\n" 
		    buff_adj (fst (Ports.min_elt e.p)) (fst (Ports.max_elt e.p))
		else   
		  (* inner name -> port *)
		  sprintf "%si%s -> v%d [ arrowhead=dot ];\n" 
		    buff_adj (string_of_name (Face.choose e.i)) (fst (Ports.choose e.p))
              else    
		(* port -> outer name *)
		sprintf "%sv%d -> o%s [ dir=back, arrowtail=dot ];\n" 
		  buff_adj (fst (Ports.choose e.p)) (string_of_name (Face.choose e.o)))    
          )       
        )
      ) l (0, "", "", "", "edge [ color=green, arrowhead=none, arrowsize=0.5 ];\n") with
  | (_, a, b, c, d) -> (a, b, c, d)

(* decompose t. p is assumed epi and mono. Ports are normalised.
   i_c and i_d are isos from t to c and d.*)
let decomp t p i_e i_c i_d f_e =
  (* compute sets of nodes in c and d *)
  let (v_c, v_d) = 
    (IntSet.of_list (Iso.dom i_c), IntSet.of_list (Iso.dom i_d)) 
  (* Introduce indices *)
  and t_a = Array.of_list (Lg.elements t)
  and p_a = Array.of_list (Lg.elements p) 
  (* Inverse isos: T -> P *)
  and i_e' = Iso.inverse i_e
  and f_e' = Rel.inverse f_e in (* Not a function *) 
  (* Domains: disjoint subsets of T *)
  let closed_t = IntSet.of_list (Iso.dom i_e')
  and non_empty_t = IntSet.of_list (Rel.dom f_e') 
  in
  (* Split every edge indexed by n in edges in d, edges in c, id. *)
  let (c, d, b_id, _) = 
    Array.fold_left (fun (acc_c, acc_d, acc_id, n) e ->
        (* n is an edge in a match with an edge in P *)
        if IntSet.mem n closed_t then (
          (acc_c, acc_d, acc_id, n + 1)
        ) 
        else (
          (* n needs to be split *) 
          let p_d = Ports.filter (fun (x, _) -> 
              IntSet.mem x v_d
            ) e.p
          and p_c = Ports.filter (fun (x, _) -> 
              IntSet.mem x v_c
            ) e.p
          and (in_c, out_d) =
            (* n is a link/edge in a match with link in P *)
            if IntSet.mem n non_empty_t then (
              let match_p = 
                IntSet.fold (fun i acc ->
                    Lg.add  p_a.(i) acc
                  ) (Rel.find n f_e') Lg.empty in
              (outer match_p, inner match_p)
            ) else
              (Face.empty, Face.empty) 
          in
          (* d and p *)
          if Face.is_empty e.o && Ports.is_empty p_c &&
             Face.is_empty in_c then
            (acc_c, 
             Lg.add { i = e.i; o = out_d; p = Ports.apply p_d i_d} acc_d, 
             acc_id, 
             n + 1)
          else 
          (* c and p *)
          if Face.is_empty e.i && Ports.is_empty p_d then
            (Lg.add { i = in_c; o = e.o; p = Ports.apply p_c i_c} acc_c, 
             acc_d,
             acc_id, 
             n + 1)
          else (
            (* id *)
            let name = Face.singleton (Nam (sprintf "~%d" n)) in
            (Lg.add { i = Face.union name in_c; 
                      o = e.o; 
                      p = Ports.apply p_c i_c } acc_c, 
             Lg.add { i = e.i; 
                      o = Face.union name out_d; 
                      p = Ports.apply p_d i_d } acc_d, 
             Lg.add { i = name; o = name; p = Ports.empty } acc_id, 
             n + 1)
          )
        )
      ) (Lg.empty, Lg.empty, Lg.empty, 0) t_a in
  (* printf "---- Decomposition\n\ *)
  (*         T  : %s\n\ *)
  (*         P  : %s\n\ *)
  (*         iso_e : %s\n\ *)
  (*         map_e : %s\n\ *)
  (*         -----\n\ *)
  (*         c  : %s\n\ *)
  (*         d  : %s\n\ *)
  (*         id : %s%!\n" *)
  (*   (to_string t) (to_string p) (Iso.to_string i_e) (Iso.to_string f_e) *)
  (*   (to_string c) (to_string d) (to_string b_id); *)
  (c, d, b_id)
    
(* Compute the levels of l. ps is a list of port levels (leaves are the last
   element). The output is a wiring and a list of link graphs. *)
let levels l ps =
  (Lg.fold (fun e acc ->
       Lg.add { i = 
	          Face.union e.i (Ports.fold (fun (n, p) acc ->
	              Face.add (Nam (sprintf "n%d_%d" n p)) acc) e.p Face.empty);
                o = e.o;
                p = Ports.empty} acc) l Lg.empty,
   fst (List.fold_left (fun (acc, in_f) lvl ->
       (elementary_id in_f) :: acc,
       Face.union in_f (Ports.fold (fun (n, p) acc ->
           Face.add (Nam (sprintf "n%d_%d" n p)) acc) lvl Face.empty))
       ([], inner l) (List.rev ps))) 

let max_ports l = 
  Lg.fold (fun e max -> 
      let max' = Ports.cardinal e.p in
      if max' > max then max' else max 
    ) l 0 

let closed_edges l = Lg.filter is_closed l

let filter_iso f l =
  let (l', _, _, iso) =
    Lg.fold (fun e (acc, i, i', iso) ->
        if f e then (Lg.add e acc, i + 1, i' + 1, Iso.add i' i iso)
        else (acc, i + 1, i', iso)
      ) l (Lg.empty, 0, 0, Iso.empty) in
  (l', iso)
  
let closed_edges_iso l =
  (* Iso from closed link graph to full link graph *)
  filter_iso is_closed l

let open_edges l = Lg.filter (fun e -> not (is_closed e)) l

let open_edges_iso l =
  filter_iso (fun e -> not (is_closed e)) l

(* Two edges are compatible if they have the same number of ports with equal
   control. *)
let compat_edges e_p e_t n_t n_p =
  Ports.types e_p.p n_p = Ports.types e_t.p n_t

exception NOT_TOTAL

(* Closed edges in p are matched to closed edges in t. Controls are checked to
   exclude incompatible pairs. Return blocking pairs and blocking columns. *)
let match_edges t p n_t n_p =
  let (clauses, _, acc_c, acc_b) = 
    Lg.fold (fun e_p (acc, i, acc_c, acc_b) ->
        let (clause, js, b, _) = 
	  Lg.fold (fun e_t (acc, js, b, j) ->
	      if compat_edges e_p e_t n_t n_p then 
	        (Cnf.P_var (Cnf.M_lit (i, j)) :: acc, j :: js, b, j + 1)
	      else (acc, js, (i, j) :: b, j + 1)
	    ) t ([], [], [], 0) in
        match js with
        | [] -> raise NOT_TOTAL (* No compatible edges found *)
        | _ -> (clause :: acc, i + 1, acc_c @ js, acc_b @ b)
      ) p ([], 0, [], []) in
  (clauses, 
   IntSet.diff (IntSet.of_int (Lg.cardinal t)) (IntSet.of_list acc_c), 
   Cnf.blocking_pairs acc_b)

let _match_ports t p n_t n_p clauses : Cnf.clause list list =
  (* printf "-------------------- _match_ports -------------------\n"; *)
  List.fold_left (fun acc e_match ->
      let (e_i, e_j) = Cnf.to_ij e_match in
      let formulas =
        Ports.compat_list p.(e_i) t.(e_j) n_p n_t in
      let res = Cnf.impl (Cnf.M_lit (e_i, e_j)) formulas in
      (* printf "%s\n" (String.concat "\n" (List.map (fun f -> *)
      (*      sprintf "(%d, %d) -> (%s)" e_i e_j  *)
      (*        (String.concat "or" (List.map (fun l -> *)
      (*             match l with *)
      (*             | Cnf.M_lit (i, j) -> sprintf "(%d, %d)" i j *)
      (*             | Cnf.V_lit _ -> assert false *)
      (*           ) f) *)
      (*        ) *)
      (*   ) formulas)); *)
      res :: acc
    ) [] (List.flatten clauses) 

(* Nodes of matched edges are isomorphic. Indexes in clauses are for closed
   edges. *)
let match_ports t p n_t n_p clauses : Cnf.clause list list =
  let a_t = 
    Array.of_list (
      List.map (fun e -> 
          e.p
        ) (Lg.elements t)
    ) 
  and a_p = 
    Array.of_list (
      List.map (fun e -> 
          e.p
        ) (Lg.elements p)
    ) in
  _match_ports a_t a_p n_t n_p clauses

(* Is p sub-hyperedge of t? *)
let sub_edge p t n_t n_p =
  let types_p = Ports.types p.p n_p
  and types_t = Ports.types t.p n_t in
  (* match with the minimum type and remove *)
  let rec find_min t ps_t acc =
    match ps_t with
    | [] -> raise Not_found
    | t' :: rs -> begin
        if Str.string_match (Str.regexp_string t) t' 0 then
	  acc @ rs
        else find_min t rs (t' :: acc) 
      end in
  (* for each type in p find the minimum matching type in t *)
  let rec scan_p ps_p ps_t =
    match ps_p with
    | [] -> true
    | t :: rs -> begin
        try
	  let ps_t' = find_min t ps_t [] in
	  scan_p rs ps_t'
        with
        | Not_found -> false 
      end in
  scan_p types_p types_t

(* Return a list of clauses on row i of matrix t. Cnf.impl will process each
   element *)
let compat_clauses e_p i t h_t n_t n_p =
  let p = Ports.to_IntSet e_p.p 
  and iso_p = Ports.arities e_p.p n_p in
  IntSet.fold (fun j acc ->
      let e_t = Hashtbl.find h_t j in
      let iso_t = Ports.arities e_t.p n_t in
      let clauses : Cnf.lit list list = 
        IntSet.fold (fun v acc ->
	    let c_v = Nodes.find n_p v 
	    and arity_v = List.assoc v iso_p 
	    and p_t = Ports.to_IntSet e_t.p in	    
	    (* find nodes in e_t that are compatible with v *)
	    let compat_t = 
	      IntSet.filter (fun u ->
	          (Ctrl.(=) c_v (Nodes.find n_t u)) &&
	          (arity_v <= (List.assoc u iso_t))
	        ) p_t in
	    let nodes_assign =
	      IntSet.fold (fun j acc -> 
	          Cnf.M_lit (v, j) :: acc
	        ) compat_t [] in
	    nodes_assign :: acc) p [] in
      (Cnf.M_lit (i, j), clauses) :: acc
    ) t []

let port_subsets p_i_list j p_a t_edge n_t n_p : Cnf.clause list = 
  let subsets xs = 
    List.fold_right (fun x rest -> 
        rest @ List.map (fun ys -> x :: ys) rest
      ) xs [[]] in
  let blocks = List.filter (fun l ->
     (* match l with 
      | [] -> false
      | _  -> ( *)
          let p_set = List.fold_left (fun acc i ->
              Ports.union acc p_a.(i).p
            ) Ports.empty l in
          not (
            sub_edge {i = Face.empty; 
                      o = Face.empty; 
                      p = p_set} t_edge n_t n_p
          )
       (* ) *)
    ) (subsets p_i_list) in
  List.map (fun l ->
      List.map (fun i ->
          Cnf.N_var (Cnf.M_lit (i, j))
        ) l
    ) blocks
    
(* Generate constraints to block sets of edges in P that cannot be matched 
   to a link in T. Example: {A, B} -> [{A}, {B}, {A, B}] blocks [{A}, {A, B}],
   [{B}, {A, B}] and [{A}, {B}, {A, B}] *)

(* 3 -> 0,2,4 *)
(* !(0,3) | ! (4,3) | !(2,3) *)
(* !(0,3) | ! (4,3) *)
(* !(2,3) | ! (4,3) *)

let compat_sub p t f_e n_t n_p =
  let p_a = Array.of_list (Lg.elements p)
  and t_a = Array.of_list (Lg.elements t) in
  fst (
    Hashtbl.fold (fun j i (acc, marked) ->
        if List.mem j marked then (acc, marked)
        else (
          let p_i_list = Hashtbl.find_all f_e j in
          let p_set = List.fold_left (fun acc i ->
              Ports.union acc p_a.(i).p
            ) Ports.empty p_i_list in
          if sub_edge {i = Face.empty; 
                       o = Face.empty; 
                       p = p_set} t_a.(j) n_t n_p 
          then (acc, marked)
          else (
            let clauses = port_subsets p_i_list j p_a t_a.(j) n_t n_p in
            (clauses @ acc, j :: marked)
          )
        )
      ) f_e ([], [])
  )
  
(* Peers in the pattern are peers in the target. Auxiliary variables are
   introduced to model open edges matchings. They are stored in matrix t *)
let match_peers t p n_t n_p =
  let (open_p, iso_p) = 
    filter_iso (fun e ->
        not (Ports.is_empty e.p) && (not (is_closed e))
      ) p
  and (non_empty_t, iso_open) = 
    filter_iso (fun e ->  not (Ports.is_empty e.p)) t in
  let h = Hashtbl.create (Lg.cardinal non_empty_t) in
  ignore (Lg.fold (fun e i ->
      Hashtbl.add h i e;
      i + 1) non_empty_t 0);
  let r = Lg.cardinal open_p
  and c = Lg.cardinal non_empty_t in
  let f_e = Hashtbl.create (r * c) in (* T -> P *)
  let c_s = IntSet.of_int c in
  let (f, block, _) =
    Lg.fold (fun e_p (acc, block, i) ->
        (* Find compatible edges in the target *)
        let (_, compat_t) = 
	  Lg.fold (fun e_t (j, acc) ->
	      if sub_edge e_p e_t n_t n_p then ( 
                Hashtbl.add f_e j i;	
                (j + 1, IntSet.add j acc)
              ) else
	        (j + 1, acc)
	    ) non_empty_t (0, IntSet.empty) in
        (* No compatible edges found *)
        if IntSet.is_empty compat_t then
	  raise NOT_TOTAL
        else (
	  (* Generate possible node matches for every edge assignment. *)
	  let clauses = 
	    List.map (fun (l, r) ->
	        Cnf.impl l r
	      ) (compat_clauses e_p i compat_t h n_t n_p)
	  (* Blockig pairs *)
	  and b =
	    IntSet.fold (fun j acc ->
	        (i, j) :: acc
              ) (IntSet.diff c_s compat_t) [] in
	  (clauses @ acc, b @ block, i + 1)
        )
      ) open_p ([], [], 0) in
  let block_f = compat_sub open_p non_empty_t f_e n_t n_p in
  (r, c, f, block, block_f, iso_p, iso_open)

let edg_iso a b n_a n_b  = 
  (Face.equal a.i b.i) && (Face.equal a.o b.o) &&
  (Ports.types a.p n_a = Ports.types b.p n_b) (* Shared *)

let key e =
  (Face.cardinal e.i, Ports.cardinal e.p, Face.cardinal e.o)

(* Partition edges according to cardinalities of faces and port sets. 
   Return a hastbl : key -> (edge, index) *)
let partition_edg l =
  let h = Hashtbl.create (Lg.cardinal l) in
  ignore (
    Lg.fold (fun e i ->
        let k = key e in 
        Hashtbl.add h k (e, i);
        i + 1
      ) l 0
  );
  h

(* P -> T Example constraint:
   [[(1, 2) or (1, 3) or (1, 4)]; [(2, 4)]; [(3, 4) or (3, 2)]] *)
let match_list_eq p t n_p n_t : Cnf.clause list  * Cnf.clause list =
  let h = partition_edg t in
  let (clauses, b, _) = 
    Lg.fold (fun e_p (acc, block, i) ->
        let t_edges = Hashtbl.find_all h (key e_p) in
        let clause = List.fold_left (fun acc (e_t, j) ->
	    if edg_iso e_t e_p n_t n_p then 
	      (Cnf.P_var (Cnf.M_lit (i, j))) :: acc
            else acc
          ) [] t_edges in
        match clause with
        | [] -> (acc, i :: block, i + 1 )
        | _ -> (clause :: acc, block, i + 1)
      ) p ([], [], 0) in
  (clauses, Cnf.block_rows b (Lg.cardinal t))

let match_ports_eq p t n_p n_t clauses : Cnf.clause list list =
  let array_t = 
    Array.of_list (List.map (fun e -> e.p) (Lg.elements t)) 
  and array_p = 
    Array.of_list (List.map (fun e -> e.p) (Lg.elements p)) in
  _match_ports array_t array_p n_t n_p clauses


 
