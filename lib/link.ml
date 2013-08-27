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
type edg = {i: Face.t; o: Face.t; p: Ports.t}

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

module Quad = 
  Set.Make(struct 
    type t = (int * int * int * int)
    let compare = fun (a, b, c, d) (x, y, z, w) ->
      match a - x with
      | 0 -> begin
	match b - y with
	| 0 -> begin
	  match c - z with
	  | 0 -> d - w
	  | v -> v
	end
	| v -> v
      end
      | v -> v
  end)

(* tensor product fails (inner common names, outer common names)*)
exception NAMES_ALREADY_DEFINED of (Face.t * Face.t)

(* Composition fails *)
exception FACES_MISMATCH of (Face.t * Face.t)

let string_of_name (Nam s) = s

let parse_face =
  List.fold_left (fun acc x -> Face.add (Nam x) acc) Face.empty

let string_of_face f =
  sprintf "{%s}"
    (String.concat ", " (List.map string_of_name (Face.elements f))) 
  
let string_of_edge e =
  sprintf "(%s, %s, %s)" 
    (string_of_face e.i) (string_of_face e.o) (Ports.to_string e.p)

let to_string l = 
  String.concat "\n" (List.map string_of_edge (Lg.elements l))

let snf_of_linking l =
  String.concat " || " (List.map (fun e ->
    sprintf "%s/%s" (string_of_face e.o) (string_of_face e.i)) (Lg.elements l))

(* Elementary substitution: one edge without ports *)
let elementary_sub f_i f_o =
  if Face.is_empty f_i && Face.is_empty f_o then Lg.empty
  else Lg.singleton {i = f_i; o = f_o; p = Ports.empty}

(* Node index is 0. Ports are from 0 to |f| - 1 *)
let elementary_ion f =
  fst (Face.fold (fun n (acc, i) ->
    let e = Lg.singleton { i = Face.empty;
			   o = Face.singleton n;
			   p = Ports.singleton (0, i);
			 } in
    (Lg.union e acc, i + 1)) f (Lg.empty, 0))

let inner (lg : Lg.t) =
  Lg.fold (fun e acc -> Face.union e.i acc) lg Face.empty

let outer (lg : Lg.t) =
  Lg.fold (fun e acc -> Face.union e.o acc) lg Face.empty

let ports lg = 
  Lg.fold (fun e acc -> Ports.union e.p acc) lg Ports.empty
	
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
  in if Face.is_empty i_in && Face.is_empty i_out 
    then Lg.union lg0 (offset lg1 n0)
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
	 sprintf "%si%s [shape=plaintext, label=\"%s\", width=.18, height=.18];\n"
           buff (string_of_name n) (string_of_name n)) e.i buff_i,
      (* Outer names *)  
       Face.fold (fun n buff ->
	 sprintf "%so%s [shape=plaintext, label=\"%s\", width=.18, height=.18];\n"
           buff (string_of_name n) (string_of_name n)) e.o buff_o,
      (* Hyperedges *)   
       (if flag then sprintf
	   "%se%d [shape=point, label=\"\", width=.0, height=.0, style=filled, color=green];\n"
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
		  sprintf "e%d -> v%d [dir=back, arrowtail=tee, weight=5];\n"
		    i (fst (Ports.choose e.p))
               else Ports.fold (fun (v, _) buff ->
		 sprintf "%se%d -> v%d;\n" buff i v) e.p ""))  
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
		  sprintf "%sv%d -> v%d;\n" buff_adj (fst (Ports.min_elt e.p))
		    (fst (Ports.max_elt e.p))
		else   
           (* inner name -> port *)
		  sprintf "%si%s -> v%d;\n" buff_adj (string_of_name (Face.choose e.i))
		    (fst (Ports.choose e.p))
              else    
         (* port -> outer name *)
		sprintf "%sv%d -> o%s;\n" buff_adj (fst (Ports.choose e.p))
		  (string_of_name (Face.choose e.o)))    
       )       
      )
    ) l (0, "", "", "", "edge [color=green, arrowhead=none];\n") with
    | (_, a, b, c, d) -> (a, b , c, d)
  
(* decompose t. p is assumed epi and mono. Ports are normalised.
   i_c and i_d are isos from t to c and d.*)
let decomp t p i_n i_e i_c i_d =
  (* compute sets of nodes in c and d *)
  let (v_c, v_d) = (Iso.dom i_c, Iso.dom i_d) 
  (* Introduce indices *)
  and t_a = Array.of_list (Lg.elements t)
  and p_a = Array.of_list (Lg.elements p) in
  (* normalise iso on all links not just edges *)
  let i_e_norm = 
    let norm a =
      let iso, _, _ = Array.fold_left (fun (acc, i, j) e ->
	if is_closed e then (Iso.add (i, j) acc, i + 1, j + 1) 
	else (acc, i, j + 1)) (Iso.empty, 0, 0) a in
      iso in
    let iso_p = norm p_a
    and iso_t = norm t_a in
    Iso.fold (fun (a, b) acc ->
    Iso.add (get_i a iso_p, get_i b iso_t) acc) i_e Iso.empty in
  (* construct equivalence classes on p's faces *)
  (*and eq_out, eq_in = eq_names p in*) 
  (*printf "eq_out: {%s}\neq_in: {%s}\n" 
    (String.concat "," (List.map (fun x -> string_of_face x) (Face_set.elements eq_out)))
    (String.concat "," (List.map (fun x -> string_of_face x) (Face_set.elements eq_in)));*)
  (* Powerset construction: transform p -> t into t -> {p} *)
  (*and h = hash_of_iso (inverse i_e) in*) 
  (* Split every edge indexed by n in edges in d, edges in c, id. *)
  let vect = Array.mapi (fun n e ->
    let p_d = Ports.filter (fun (x, _) -> Int_set.mem x v_d) e.p
    and p_c = Ports.filter (fun (x, _) -> Int_set.mem x v_c) e.p
    and p_p = Ports.filter (fun (x, _) -> Int_set.mem x (codom i_n)) e.p in
    (* Interface of id *)
    let f_id = 
      if ((Ports.equal e.p p_c) && (Face.is_empty e.i)) || (* e is in c *)
	((Ports.equal e.p p_d) && (Face.is_empty e.o)) || (* e is in d *)
	(* e is ONE edge in p *)
	((Ports.equal e.p p_p) (*&& (List.length (Hashtbl.find_all h n) < 2)*)) (* e is in p *)    
      then Face.empty
      else Face.singleton (Nam (sprintf "id%d" n)) in
    (* Mediating interfaces of d and c *) 
    (* Find liks in p having ports in common with e *)
      let edges_p =  (* FIX THIS *)
	(* if it's a matched edge no names are required *)
	if Int_set.mem n (codom i_e_norm) then Lg.empty
	else Lg.filter (fun e_p ->
	sub_multi (card_ports p_p) (card_ports e_p.p)) (Lg.filter (fun e_p ->
	  Ports.exists (fun (x, _) ->
	    Int_set.mem (get_i x i_n) (set_of_ports p_p)) e_p.p) p) in
      (*multiset_of_ports e.p*)
      let i_c = outer edges_p 
      and o_d = inner edges_p in
      ({i = Face.union f_id i_c; o = e.o; p = p_c},
     {i = e.i; o = Face.union f_id o_d; p = p_d},
     {i = f_id; o = f_id; p = Ports.empty})
  ) t_a in					
  (* Build link graphs by removing empty edges*)
  let (u_c, u_d, id) =
    Array.fold_left (fun (acc_c, acc_d, acc_id) (c, d, id) ->
      let aux e acc =
	if (Face.is_empty e.i) && (Face.is_empty e.o) && (Ports.is_empty e.p)
	then acc
	else Lg.add e acc in
      (aux c acc_c, aux d acc_d, aux id acc_id)
    ) (Lg.empty, Lg.empty, Lg.empty) vect in
  (* Normalise ports *) 	  
  (apply_iso i_c u_c, apply_iso i_d u_d, id)	  

(* Compute the levels of l. ps is a list of port levels (leaves are the last
   element). The output is a wiring and a list of link graphs. *)
let levels l ps =
  (Lg.fold (fun e acc ->
    Lg.add {i = Face.union e.i (Ports.fold (fun (n, p) acc ->
      Face.add (Nam (sprintf "n%d_%d" n p)) acc) e.p Face.empty);
            o = e.o;
            p = Ports.empty} acc) l Lg.empty,
   fst (List.fold_left (fun (acc, in_f) lvl ->
     (elementary_id in_f) :: acc,
     Face.union in_f (Ports.fold (fun (n, p) acc ->
       Face.add (Nam (sprintf "n%d_%d" n p)) acc) lvl Face.empty)
   ) ([], inner l) (List.rev ps))) 

let close_edges l = Lg.filter is_closed l

(* does edge e contain ports from node i? *)
let is_linked_to e i = 
  Ports.exists (fun (x, _) -> x = i) e.p

(* nodes linked via some hyperedge to node i *)
let peers l i =
  Lg.fold (fun e acc ->
    if is_linked_to e i then
      let ports_i = (* remove one i-port *) 
	Ports.remove (Ports.max_elt (Ports.filter (fun (x, _) -> 
	  x = i) e.p)) e.p in
      Int_set.union acc (set_of_ports ports_i)
    else acc) l Int_set.empty

let are_peers l i j =  Int_set.mem j (peers l i)

(* if two nodes are peers in the pattern, they have to be peers in the target:
   forbid assignments with non-peers. *)
let non_peers_pairs l v = 
  Int_set.fold (fun i acc -> 
    let non_peers = Int_set.diff v (peers l i) in
    let a = Int_set.fold (fun x acc ->
      Iso.add (i, x) acc) non_peers Iso.empty in
    Iso.union acc a) v Iso.empty (* inverse a???*)

let match_peers t p m n =
  (* merge a pair of peers with a set of non-peers pairs *)
  let aux i j s = 
    Iso.fold (fun (a, b) acc ->
      Quad.add (i, j, a, b) acc) s Quad.empty
  and v_t, v_p = of_int m, of_int n in
  let pairs_t = non_peers_pairs t v_t in
  (*printf "non_peers_pairs in t = {%s}.\n" (String.concat ", " (List.map (fun (a, b) ->
  sprintf "(%d,%d)" a b) (Iso.elements pairs_t)));*)
  Quad.elements (Int_set.fold (fun i acc0 ->
    Quad.union acc0 (Int_set.fold (fun j acc1 ->
      if are_peers p i j then
	((*printf "%d and %d are peers.\n" i j;*)
	Quad.union acc1 (aux i j  pairs_t))
      else acc1) v_t Quad.empty)) v_p Quad.empty)
   
(*let open_edges l = Lg.diff l (close_edges l)*)

(* generates a list of unmatchable nodes = blocking pairs *)
let gen_blocking_pairs_edges e_t e_p = 
  let m_t = multiset_of_ports e_t.p
  and m_p = multiset_of_ports e_p.p in
  Iso.fold (fun (n, i) acc ->
    (List.map (fun (_, j) -> (i, j)) (Iso.elements (Iso.filter (fun (m, _) ->
      m <> n) m_t))) @ acc)  m_p []

exception NO_ISO

(* generate a list of possible isos - incompatible ctrl are ignored *)
let gen_isos_edges e_t e_p block_ctrl = 
  let m_t = multiset_of_ports e_t.p
  and m_p = multiset_of_ports e_p.p in
  try
    (* List of possible pairs ((a,1), (a,2) ....); (b,3), (b,4) ....) *)
    let pairs = List.map (fun (i, js) -> 
      Int_set.fold (fun j acc ->
	Iso.add (i, j) acc) js Iso.empty) (Iso.fold (fun (c, i) acc ->
	  (i, 
	   let js = Int_set.diff 
	     (Iso.fold (fun (_, j) acc -> 
	       Int_set.add j acc) (Iso.filter (fun (d, _) -> 
		 d = c) m_t) Int_set.empty)
	     (codom (Iso.filter (fun (x, _) -> x = i) block_ctrl)) in
	   (* if one set is empty then the two edges can't be matched *)
	   if Int_set.is_empty js then raise NO_ISO
	   else js) :: acc) m_p []) in
    let out =  cart_of_list_iso pairs in
    (*printf "gen_isos_edges [%s]\n" (String.concat "; " (List.map string_of_iso out));
      printf "block_ctrl %s\n" (string_of_iso block_ctrl);*)
    out
  with
    | NO_ISO -> []

(* convert to cnf (e_i,e_j) -> (iso0 | iso1 |  ....) *)
let to_cnf i j isos =
  List.map (fun iso ->
  ((i, j), iso)) (cart_of_list_iso isos)

(* generates the constraint for edges: !(e,e) v !(v,v) *)
let gen_constraint_edges e_t e_p j i =
  List.map (fun (v, w) -> (i, j, v, w)) (gen_blocking_pairs_edges e_t e_p)

(* same number of ports *)
let match_edges t p block_ctrl =
  let close_t = close_edges t in
  snd (Lg.fold (fun e_p (i, (acc_p, acc_v, acc_e)) ->
    i + 1, 
    (let  _, ports, c, e = 
       Lg.fold (fun e_t (j, c_ports, cnstr, acc) ->
	 if (Ports.cardinal e_p.p <> Ports.cardinal e_t.p) ||
	   (card_ports e_p.p <> card_ports e_t.p) then
	   (j + 1, c_ports, cnstr, Iso.add (i, j) acc)
	 else
	   begin	   
	     let isos_ports = to_cnf i j (gen_isos_edges e_t e_p block_ctrl) in
	     match isos_ports with
	       | [] -> (j + 1, c_ports,
			cnstr, ((*printf "negating (%d,%d)\n" i j;*) Iso.add (i, j) acc))
	       | _ -> (j + 1, c_ports @ isos_ports,
		       cnstr @ (gen_constraint_edges e_t e_p j i), acc)
	   end) close_t (0, [], [], Iso.empty) in
     acc_p @ ports, acc_v @ c, Iso.union acc_e e)) (close_edges p) (0, ([], [], Iso.empty)))
 
let is_sub_hyper a b = 
  let a_c = card_ports a.p
  and b_c = card_ports b.p in
  let rec aux p t = 
    match p with
      | [] -> true
      | x :: xs -> 
	let new_t = fst (List.partition (fun y -> y >= x) t) in
	match new_t with
	  | [] -> false
	  | _ -> aux xs (List.tl new_t) in
  aux a_c b_c

let block_links e_p t =
  let nodes_t = 
    Lg.fold (fun e acc ->
      Int_set.union acc (set_of_ports e.p)) t Int_set.empty in
  Int_set.fold (fun i acc ->
    Iso.union acc (Int_set.fold (fun j acc -> 
      Iso.add (i, j) acc) nodes_t Iso.empty)
  ) (set_of_ports e_p.p) Iso.empty

(* block if a pattern link can't be contained in a target link *)
let match_links t p = 
  Lg.fold (fun e_p (b_n, b_e) ->
    if is_closed e_p then (b_n, b_e)
    else (
      if Lg.exists (fun e_t ->
	is_sub_hyper e_p  e_t) t then (b_n, b_e)
      else 
	(Iso.union b_n (block_links e_p t), b_e)
    )
  ) p (Iso.empty, Iso.empty)

(* return blocking pairs of links with different interfaces or number of 
   ports *)
let match_link_pairs a b n_a n_b =
  let aux s = 
    Array.of_list (Lg.elements s) in
  let vec_a = aux a 
  and vec_b = aux b in
  let res = ref [] in
  for i = 0 to (Array.length vec_a) - 1 do
    for j = 0 to (Array.length vec_b) - 1 do
      if (Face.equal (vec_a.(i).i) (vec_b.(j).i)) &&
	(Face.equal (vec_a.(i).o) (vec_b.(j).o)) &&
	((type_of_ports (vec_a.(i).p) n_a) = (type_of_ports (vec_b.(j).p) n_b))
      then ()
      else res := (i, j) :: !res
    done
  done;
  !res
    
(*let is_match_valid t p iso =
  true*)

(* DEBUG *)
(*let _ =
  let aux = 
    List.fold_left (fun s f -> Face_set.add f s) Face_set.empty in
  let a = aux [parse_face ["a"]; parse_face ["b"]; parse_face ["c"; "d"]]
  and b = aux [parse_face ["a"; "b" ; "c"]; parse_face ["d"]] in
  let res1 = equiv_class a b in
  printf "equiv_class:\n";
  Face_set.iter (fun f -> printf "%s\n" (string_of_face f)) res1;
  let c = aux [parse_face ["a"; "b"]; parse_face ["c"]; parse_face ["d"]]
  and d = aux [parse_face ["a"]; parse_face ["b"; "c"]; parse_face ["d"]] in
  let res2 = equiv_class c d in
  printf "equiv_class:\n";
  Face_set.iter (fun f -> printf "%s\n" (string_of_face f)) res2;
  let parse =
   List.fold_left (fun s f -> Lg.add f s) Lg.empty
  and parse_p =
   List.fold_left (fun s f -> Ports.add f s) Ports.empty in
  let a = parse
    [{o = parse_face ["a";"b";"c"];
      i = parse_face ["a"];
      p = parse_p [(0,0);(0,1)]}; 
     {o = parse_face ["d"];
      i = parse_face ["b"];
      p = parse_p [(1,0)]};
     {o = parse_face [];
      i = parse_face ["c";"d"];
      p = parse_p []};
     {o = parse_face ["e"];
      p = parse_p [(2,0)];
      i = parse_face []}]
  and b  = parse
    [{i = parse_face ["a"];
      o = parse_face ["a";"b";"c"];
      p = parse_p [(0,0)]};
     {i = parse_face [];
      o = parse_face ["d"];
      p = parse_p []};
     {i = parse_face [];
      o = parse_face [];
      p = parse_p [(0,1);(0,2)]} ] in
  printf "a = \n%s\n" (string_of_lg a);
  printf "b = \n%s\n" (string_of_lg b);
  printf "comp:\n%s\n" (string_of_lg (comp a b 3));
  printf "tens:\n%s\n" (string_of_lg (ppar a b 3));
  let t = parse 
    [{o = parse_face ["x"];
      i = parse_face [];
      p = parse_p [(0,0);(3,0);(4,0);(5,0)]};
     {o = parse_face [];
      i = parse_face [];
      p = parse_p [(1,0);(7,0)]};
     {o = parse_face [];
      i = parse_face [];
      p = parse_p [(2,0)]};
     {o = parse_face [];
      i = parse_face [];
      p = parse_p [(6,0)]}]
  and p = parse
    [{o = parse_face ["x"];
      i = parse_face [];
      p = parse_p [(0,0)]}; 
     {o = parse_face ["y"];
      i = parse_face [];
      p = parse_p [(2,0)]}; 
     {o = parse_face [];
      i = parse_face ["z"];
      p = parse_p [(1,0)]};] in
  printf "t = \n%s\n" (string_of_lg t);
  printf "p = \n%s\n" (string_of_lg p); 
  let (c, d, id) =
    decomp t p (of_list [(0,0);(1,1);(2,4)]) (of_list [(0,3);(1,3);(2,0)])
      (of_list []) (of_list [(2, 0);(3, 1);(5, 2);(6, 3);(7, 4)]) in
  printf "c = \n%s\n" (string_of_lg c);
  printf "d = \n%s\n" (string_of_lg d);
  printf "id = \n%s\n" (string_of_lg id);
  printf "re-comp =\n%s\n" (string_of_lg 
    (comp c (comp (tens id  p 0) d 3) 0));
  let (w, ls) = levels t
    [parse_p [(0,0)]; parse_p [(1,0);(2,0)]; parse_p [(3,0);(4,0);(5,0)];
    parse_p [(6,0);(7,0)]] in
  printf "wiring = \n%s\n" (string_of_lg w);
  List.iter (fun l -> printf "level =\n%s\n" (string_of_lg l)) ls;      
*)   
  
