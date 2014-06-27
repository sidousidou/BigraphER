open Utils
open Printf

module Face = Set.Make (
  struct
    type t = string
    let compare = String.compare
  end)

(* Module used to compute equivalence classes *)
module FaceSet = Set.Make (Face)

(* Edges *)
type edg = { 
  i: Face.t;      (* Inner names *) 
  o: Face.t;      (* Outer names *)  
  p: int Port.t;  (* Ports : ide -> #of ports *)
}

let edg_compare (h : edg) (k : edg) =
  match Face.compare h.i k.i with
  | 0 -> begin
      match Face.compare h.o k.o with
      | 0 -> Port.compare int_compare h.p k.p
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

let parse_face =
  List.fold_left (fun acc x -> 
      Face.add x acc) Face.empty

let string_of_face f =
  sprintf "{%s}" (String.concat ", " (Face.elements f)) 

let string_of_edge e =
  sprintf "(%s, %s, %s)" 
    (string_of_face e.i) (string_of_face e.o) (Port.to_string e.p)

let to_string l = 
  String.concat "\n" (List.map string_of_edge (Lg.elements l))

(* Nodes are counted starting from 1 in the input. No inner names. 
   Example:
            1 1 2 f
            3 4 t
            3 3 2 f 
*) 
let parse_edge s i =
  let a  = Str.split (Str.regexp_string " ") s in 
  { i = Face.empty;
    o = (match List.nth a ((List.length a) - 1) with 
        | "t" -> Face.singleton ("n" ^ (string_of_int i))
        | "f" -> Face.empty
        | _ -> assert false);
    p = List.fold_left (fun acc x -> 
        match x with
        | "t" | "f" -> acc
        | _ -> Port.union acc (Port.singleton ((int_of_string x) - 1) 1)
      ) Port.empty a;
  }

let parse lines = 
  fst (List.fold_left (fun (acc, i) s -> 
      (Lg.add (parse_edge s i) acc, i + 1)
    ) (Lg.empty, 0) lines)

(* Elementary substitution: one edge without ports *)
let elementary_sub f_i f_o =
  if Face.is_empty f_i && Face.is_empty f_o then 
    Lg.empty
  else Lg.singleton { i = f_i; o = f_o; p = Port.empty }

(* One edge with one port for each name in f *)
let elementary_ion f =
  Face.fold (fun n acc ->
      Lg.add { i = Face.empty;
               o = Face.singleton n;
               p = Port.singleton 0 1
             } acc
    ) f Lg.empty

let inner (lg : Lg.t) =
  Lg.fold (fun e acc -> 
      Face.union e.i acc) lg Face.empty

let outer (lg : Lg.t) =
  Lg.fold (fun e acc -> 
      Face.union e.o acc) lg Face.empty

(* let ports lg =  *)
(*   Lg.fold (fun e acc ->  *)
(*       Ports.union e.p acc) lg Ports.empty *)

(* Add offset m to all the port indexes *)
let offset (lg : Lg.t) (m : int) =
  Lg.fold (fun e acc ->
      Lg.add { i = e.i; 
	       o = e.o;
	       p = Port.offset e.p m;
	     } acc
    ) lg Lg.empty

(* n0 is necessary because some nodes my be present in the left place      *)
(* graph but not in the link graph.                                        *)
let tens lg0 lg1 n0 =
  if Face.is_empty (Face.inter (inner lg0) (inner lg1)) && 
     Face.is_empty (Face.inter (outer lg0) (outer lg1)) then
    Lg.union lg0 (offset lg1 n0)
  else raise (NAMES_ALREADY_DEFINED (Face.inter (inner lg0) (inner lg1),
                                     Face.inter (outer lg0) (outer lg1)))

(* Identity. One edge for every name in f *)
let elementary_id f =
  Face.fold (fun n acc ->
      Lg.add { i = Face.singleton n; 
	       o = Face.singleton n; 
	       p = Port.empty;
	     } acc
    ) f Lg.empty

let id_empty = Lg.empty

let is_id =
  Lg.for_all (fun e ->
      Face.equal e.i e.o && (Face.cardinal e.i = 1) && Port.is_empty e.p)

(* Fuse edges in a that are composable with aliases in b *)
let fuse a b =
  Lg.fold (fun e acc ->
      let (l0, l1) = Lg.partition (fun e' -> 
          Face.is_empty (Face.inter e'.i e.o)
        ) acc in
      Lg.add { i = inner l1;
               o = outer l1;
               p = Lg.fold (fun e acc ->
                   Port.union e.p acc
                 ) l1 Port.empty;
             } l0 
    ) b a

(* Composition A o B. [n] is the number of nodes in A. *)
let comp a b n = 
  let x = inner a
  and y = outer b in
  if Face.equal x y then (
    let a' = fuse a b in
    let b' = offset b n in
    let (merged, unmerged) = 
      Lg.fold (fun e (acc, rest) ->
          let (l0, l1) = Lg.partition (fun e' -> 
              Face.is_empty (Face.inter e'.o e.i)
            ) rest in 
          (Lg.add { i = inner l1;
                    o = e.o;
                    p = Lg.fold (fun e acc ->
                        Port.union e.p acc
                      ) l1 e.p;
                  } acc, l0) 
        ) a' (Lg.empty, b') in
    Lg.union merged unmerged
  ) else raise (FACES_MISMATCH (x, y))

(* no inner names that are siblings *)
let is_mono l =
  Lg.for_all (fun e -> Face.cardinal e.i < 2) l

(* no idle outer names *)
let is_epi l =
  Lg.for_all (fun e ->
      if Face.cardinal e.o > 0 then 
        (Face.cardinal e.i > 0 || Port.cardinal e.p > 0)
      else true
    ) l

let is_guard l =
  (* true if inner are connected to outer *)
  not (Lg.exists (fun e ->
      (not (Face.is_empty e.i)) && (not (Face.is_empty e.o))
    ) l)

(* Parallel product *)
let ppar a b n = 
  let b' = offset b n in
  Lg.fold (fun e acc ->
      let (l0, l1) = 
        Lg.partition (fun e' ->
            Face.is_empty (Face.inter e'.o e.o) &&
            Face.is_empty (Face.inter e'.i e.i)
          ) acc in
      Lg.add { i = Face.union (inner l1) e.i;
               o = Face.union (outer l1) e.o;
               p = Lg.fold (fun e acc ->
                   Port.union e.p acc
                 ) l1 e.p;
             } l0 
    ) b' a

let apply_exn i l =
  Lg.fold (fun e acc ->
      Lg.add { i = e.i; 
               o= e.o; 
               p = Port.apply_exn e.p i 
             } acc
    ) l Lg.empty

(* Is e a hyperedge? An extra node is not required when it is an edge or an
   idle name.*)   
let is_hyp e = 
  (* closure on a port *)
  ((Port.cardinal e.p) = 1 && (Face.is_empty e.i) && (Face.is_empty e.o)) ||
  (* idle alias on two names *)
  ((Face.is_empty e.i) && (Port.is_empty e.p) && (Face.cardinal e.o = 2)) ||  
  (* closure xon two names *)
  ((Face.is_empty e.o) && (Port.is_empty e.p) && (Face.cardinal e.i = 2)) ||  
  (* more than 2 ports or names *)
  ((Port.cardinal e.p) + (Face.cardinal e.i) + (Face.cardinal e.o) > 2)

(* is e an idle name? *)
let is_idle e = 
  (* inner name *)
  ((Face.is_empty e.o) && (Port.is_empty e.p) && (Face.cardinal e.i = 1)) ||
  (* outer name *)
  ((Face.is_empty e.i) && (Port.is_empty e.p) && (Face.cardinal e.o = 1))

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
                buff n n
            ) e.i buff_i,
          (* Outer names *)  
          Face.fold (fun n buff ->
	      sprintf "%so%s [ shape=plaintext, label=\"%s\", width=.18,\
                       height=.18, fontname=\"serif\", fontsize=9.0 ];\n"
                buff n n
            ) e.o buff_o,
          (* Hyperedges *)   
          (if flag then 
             sprintf "%se%d [ shape=point, label=\"\", width=.0, height=.0,\
                      style=invis, color=green ];\n"
	       buff_h i    
	   else buff_h),
          (* Adjacency *)
          (if flag then
	     (buff_adj ^
              (Face.fold (fun n buff ->
		   sprintf "%si%s -> e%d;\n" buff n i
                 ) e.i "") ^ 
              (Face.fold (fun n buff ->
		   sprintf "%so%s -> e%d;\n" buff n i
                 ) e.o "") ^
              (if (Port.cardinal e.p) = 1 && (Face.is_empty e.i) &&
		  (Face.is_empty e.o) then
		 (* closure from a port *)
		 sprintf "e%d -> v%d [ dir=both, arrowhead=dot, arrowtail=tee,\
                          weight=5 ];\n"
		   i (fst (Port.choose e.p))
               else Port.fold (fun v _ buff ->
		   sprintf "%se%d -> v%d [dir=both, arrowhead=dot,\
                            arrowtail=none ];\n" buff i v
                 ) e.p ""))  
	   else
             (* idle name *)
           if is_idle e then buff_adj
	   else   
             (* edge between two points *)
             (if Port.is_empty e.p then
		(* name -> name *)
		sprintf "%si%s -> o%s;\n" 
                  buff_adj (Face.choose e.i) (Face.choose e.o)
              else if Face.is_empty e.o then
		if Face.is_empty e.i then
		  (* port -> port *)
		  sprintf "%sv%d -> v%d [ dir=both, arrowtail=dot,\
                           arrowhead=dot ];\n" 
                    buff_adj (fst (Port.min_binding e.p)) (fst (Port.max_binding e.p))
		else   
		  (* inner name -> port *)
		  sprintf "%si%s -> v%d [ arrowhead=dot ];\n" 
		    buff_adj (Face.choose e.i) (fst (Port.choose e.p))
              else    
		(* port -> outer name *)
		sprintf "%sv%d -> o%s [ dir=back, arrowtail=dot ];\n" 
		  buff_adj (fst (Port.choose e.p)) (Face.choose e.o))    
          )       
        )
      ) l (0, 
           "", 
           "", 
           "", 
           "edge [ color=green, arrowhead=none, arrowsize=0.5 ];\n"
          ) with
  | (_, a, b, c, d) -> (a, b, c, d)

let safe_exn f = 
  try f with
  | Not_found -> assert false

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
  and f_e' = Fun.inverse f_e in (* Relation *) 
  (* Domains: disjoint subsets of T *)
  let closed_t = IntSet.of_list (Iso.dom i_e')
  and non_empty_t = IntSet.of_list (Rel.dom f_e') in
  (* Split every edge indexed by n in edges in d, edges in c, id. *)
  let (c, d, b_id, _) = 
    Array.fold_left (fun (acc_c, acc_d, acc_id, n) e ->
        (* n is an edge in a match with an edge in P *)
        if IntSet.mem n closed_t then (
          (acc_c, acc_d, acc_id, n + 1)
        ) 
        else (
          (* n needs to be split *) 
          let p_d = Port.filter (fun x _ -> 
              IntSet.mem x v_d
            ) e.p
          and p_c = Port.filter (fun x _ -> 
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
          if Face.is_empty e.o && Port.is_empty p_c &&
             Face.is_empty in_c then
            (acc_c, 
             Lg.add { i = e.i; 
                      o = out_d; 
                      p = safe_exn (Port.apply_exn p_d i_d);
                    } acc_d, 
             acc_id, 
             n + 1)
          else 
          (* c and p *)
          if Face.is_empty e.i && Port.is_empty p_d then
            (Lg.add { 
                i = in_c; 
                o = e.o; 
                p = safe_exn (Port.apply_exn p_c i_c);
              } acc_c, 
             acc_d,
             acc_id, 
             n + 1)
          else (
            (* id *)
            let name = Face.singleton ("~" ^ (string_of_int n)) in
            (Lg.add { i = Face.union name in_c; 
                      o = e.o; 
                      p = safe_exn (Port.apply_exn p_c i_c);
                    } acc_c, 
             Lg.add { i = e.i; 
                      o = Face.union name out_d; 
                      p = safe_exn (Port.apply_exn p_d i_d)
                    } acc_d, 
             Lg.add { i = name; o = name; p = Port.empty } acc_id, 
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
(* let levels l ps = *)
(*   (Lg.fold (fun e acc -> *)
(*        Lg.add { i =  *)
(* 	          Face.union e.i (Ports.fold (fun (n, p) acc -> *)
(* 	              Face.add (Nam (sprintf "n%d_%d" n p)) acc) e.p Face.empty); *)
(*                 o = e.o; *)
(*                 p = Ports.empty} acc) l Lg.empty, *)
(*    fst (List.fold_left (fun (acc, in_f) lvl -> *)
(*        (elementary_id in_f) :: acc, *)
(*        Face.union in_f (Ports.fold (fun (n, p) acc -> *)
(*            Face.add (Nam (sprintf "n%d_%d" n p)) acc) lvl Face.empty)) *)
(*        ([], inner l) (List.rev ps)))  *)

(* let max_ports l =  *)
(*   Lg.fold (fun e max ->  *)
(*       let max' = Ports.cardinal e.p in *)
(*       if max' > max then max' else max  *)
(*     ) l 0  *)

let closed_edges = Lg.filter is_closed

let filter_iso f l =
  let (l', _, _, iso) =
    Lg.fold (fun e (acc, i, i', iso) ->
        if f e then 
          ( Lg.add e acc, 
            i + 1, 
            i' + 1, 
            try Iso.add_exn i' i iso with
            | Iso.NOT_BIJECTIVE -> assert false
          )
        else (acc, i + 1, i', iso)
      ) l (Lg.empty, 0, 0, Iso.empty) in
  (l', iso)
  
let closed_edges_iso =
  (* Iso from closed link graph to full link graph *)
  filter_iso is_closed

let open_edges = Lg.filter (fun e -> not (is_closed e))

let open_edges_iso = filter_iso (fun e -> not (is_closed e))

(* Two edges are compatible if they have the same number of ports with equal
   control. *)
let compat_edges e_p e_t n_t n_p =
  Port.types e_p.p n_p = Port.types e_t.p n_t

(* Closed edges in p are matched to closed edges in t. Controls are checked to
   exclude incompatible pairs. Return blocking pairs and blocking columns. *)
let match_edges_exn t p n_t n_p =
  let set_t = IntSet.of_int (Lg.cardinal t) 
  and (clauses, _, acc_c, acc_b) = 
    Lg.fold (fun e_p (acc, i, acc_c, acc_b) ->
        let (clause, js, b, _) = 
	  Lg.fold (fun e_t (acc, js, b, j) ->
	      if compat_edges e_p e_t n_t n_p then 
	        (Cnf.P_var (Cnf.M_lit (i, j)) :: acc, IntSet.add j js, b, j + 1)
	      else (acc, js, (i, j) :: b, j + 1)
	    ) t ([], IntSet.empty, [], 0) in
        if IntSet.is_empty js then
          raise Exit (* No compatible edges found *)
        else (clause :: acc, i + 1, IntSet.union acc_c js, acc_b @ b)
      ) p ([], 0, IntSet.empty, []) in
  (clauses, IntSet.diff set_t acc_c, Cnf.blocking_pairs acc_b)

let _match_ports t p n_t n_p clauses : Cnf.clause list list =
  (* printf "-------------------- _match_ports -------------------\n"; *)
  List.fold_left (fun acc e_match ->
      let (e_i, e_j) = Cnf.to_ij e_match in
      let formulas =
        Port.compat_list p.(e_i) t.(e_j) n_p n_t in
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
    Array.of_list (List.map (fun e -> e.p) (Lg.elements t)) 
  and a_p = 
    Array.of_list (List.map (fun e -> e.p) (Lg.elements p)) in
  _match_ports a_t a_p n_t n_p clauses

(* Return a list of clauses on row i of matrix t. Cnf.impl will process each
   element *)
let compat_clauses e_p i t h_t n_t n_p =
  let p = Port.to_IntSet e_p.p in
  IntSet.fold (fun j acc ->
      let e_t = Hashtbl.find h_t j in
      let clauses : Cnf.lit list list = 
        IntSet.fold (fun v acc ->
	    let c_v = safe (Node.ctrl v n_p) 
	    and arity_v = safe_exn (Port.find v e_p.p) 
	    and p_t = Port.to_IntSet e_t.p in	    
	    (* find nodes in e_t that are compatible with v *)
	    let compat_t = 
	      IntSet.filter (fun u ->
	          (Ctrl.(=) c_v (safe (Node.ctrl u n_t))) &&
	          (arity_v <= (safe_exn (Port.find u e_t.p)))
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
              Port.union acc p_a.(i).p
            ) Port.empty l in
          not (
            Port.subset p_set t_edge.p n_p n_t
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
              Port.union acc p_a.(i).p
            ) Port.empty p_i_list in
          if Port.subset p_set t_a.(j).p n_p n_t 
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
let match_peers_exn t p n_t n_p =
  let (open_p, iso_p) = 
    filter_iso (fun e ->
        not (Port.is_empty e.p) && (not (is_closed e))
      ) p
  and (non_empty_t, iso_open) = 
    filter_iso (fun e ->  not (Port.is_empty e.p)) t in
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
	      if Port.subset e_p.p e_t.p n_p n_t then ( 
                Hashtbl.add f_e j i;	
                (j + 1, IntSet.add j acc)
              ) else
	        (j + 1, acc)
	    ) non_empty_t (0, IntSet.empty) in
        (* No compatible edges found *)
        if IntSet.is_empty compat_t then
	  raise Exit
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
  (Port.types a.p n_a = Port.types b.p n_b) (* Shared *)

let key e =
  (Face.cardinal e.i, Port.cardinal e.p, Face.cardinal e.o)

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

(* same as match_ports *)
(* let match_ports_eq p t n_p n_t clauses : Cnf.clause list list = *)
(*   let array_t =  *)
(*     Array.of_list (List.map (fun e -> e.p) (Lg.elements t))  *)
(*   and array_p =  *)
(*     Array.of_list (List.map (fun e -> e.p) (Lg.elements p)) in *)
(*   _match_ports array_t array_p n_t n_p clauses *)

(* Prime components decomposition *)
let prime_components lg =
  List.map (fun iso ->
      Lg.fold (fun edg acc ->
          Lg.add { edg with
                   p = Port.apply edg.p iso;
                 } acc
        ) lg Lg.empty 
    ) 
 
