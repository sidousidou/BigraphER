open Base

open Printf

(* ide strings no capital letter or number at the start *)
type name = Nam of string

module Face = Set.Make (
  struct
	  type t = name
	  let compare = fun (Nam s0) (Nam s1) -> compare s0 s1
	end)

(* Module used to compute equivalence classes *)
module Face_set = Set.Make (
  struct
    type t = Face.t
    let compare = compare
  end)

(* (in, out, ports) *)
type edg = {i: Face.t; o: Face.t; p: Ports.t}

module Lg = Set.Make (struct
		type t = edg
		let compare = fun a b ->
					match compare a.i b.i with
					| 0 -> begin
								match compare a.o b.o with
								| 0 -> Ports.compare a.p b.p
								| x -> x
							end
					| x -> x
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
 (string_of_face e.i) (string_of_face e.o) (string_of_ports e.p)

let string_of_lg l = 
  String.concat "\n" (List.map string_of_edge (Lg.elements l))

(* Elementary substitution: one edge without ports *)
let elementary_sub f_i f_o =
	if Face.is_empty f_i && Face.is_empty f_o
	then Lg.empty
	else Lg.singleton {i = f_i; o = f_o; p = Ports.empty}

(* Node index is 0. Ports are from 0 to |f| - 1 *)
let elementary_ion f =
	fst (Face.fold (fun n (acc, i) ->
							let e =
								Lg.singleton
									{i = Face.empty;
									 o = Face.singleton n;
									 p = Ports.singleton (0, i)} in
							(Lg.union e acc, i + 1))
				f (Lg.empty, 0))

let inner (lg : Lg.t) =
	Lg.fold (fun e acc -> Face.union e.i acc) lg Face.empty

let outer (lg : Lg.t) =
	Lg.fold (fun e acc -> Face.union e.o acc) lg Face.empty

let ports lg = 
  Lg.fold (fun e acc -> Ports.union e.p acc) lg Ports.empty
	
(* return a list of edges with duplicates and the number of edges *)
(*let match_string l =
	let b = Lg.fold (fun e acc ->
						let ps = Ports.fold (fun (i, _) acc ->
											acc ^ (string_of_int (i + 1)) ^ " ") e.p ""
						and c = if (Face.is_empty e.i) && (Face.is_empty e.o)
							then "f" else "t"
						in acc @ [ps ^ c]
			) l []
	in (String.concat "\n" b, Lg.cardinal l)*)

(* Add offset m to all the port indeces *)
let offset (lg : Lg.t) (m : int) =
	Lg.fold (fun e acc ->
	  Lg.add {i = e.i; o = e.o;
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
	  Lg.union (Lg.singleton
	    {i = Face.singleton n; o = Face.singleton n; p = Ports.empty})
	    acc) f Lg.empty

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
  {i = inner lg;
   o = outer lg;
   p = Lg.fold (fun e acc -> Ports.union e.p acc) lg Ports.empty}

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
  if Face.equal x y then
    (let new_b = offset b n 
    and cls_in =
      Lg.fold (fun e acc -> Face_set.add e.i acc) a Face_set.empty in
    let cls_out =
      Lg.fold (fun e acc -> Face_set.add e.o acc) new_b Face_set.empty in
    let cls = equiv_class cls_in cls_out in
    Lg.union (fuse (merge_in a cls) (merge_out new_b cls))
      (Lg.union
        (Lg.filter (fun e -> Face.is_empty e.i) a)
        (Lg.filter (fun e -> Face.is_empty e.o) new_b)))    
  else
    raise (FACES_MISMATCH (x, y))

let is_mono l =
	Lg.for_all (fun e ->
	  (Face.cardinal e.i = 1 &&
	   (Face.cardinal e.o > 0 || Ports.cardinal e.p > 0))) l

let is_epi l =
	Lg.for_all (fun e ->
	  (Face.cardinal e.o = 1 &&
	   (Face.cardinal e.i > 0 || Ports.cardinal e.p > 0))) l

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
  	Lg.add {i = e.i; o= e.o; p = apply_ports e.p i} acc) l Lg.empty

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
  let v_c, v_d = dom i_c, dom i_d 
  (* Introduce indices *)
	and t_a = Array.of_list (Lg.elements t)
	and p_a = Array.of_list (Lg.elements p)
	(* Powerset construction: transform p -> t into t -> {p} *)
	and h = hash_of_iso (inverse i_e) in
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
				 ((Ports.equal e.p p_p) && (List.length (Hashtbl.find_all h n) = 1))                                                 (* e is in p *)    
		  then Face.empty
			else Face.singleton (Nam (sprintf "%dn" n)) in
	  (* Mediating interfaces of d and c *) 
    let o_d, i_c = 
      let edges_p =
		    Int_set.fold (fun j acc ->
		      Lg.add p_a.(j) acc) (set_of_list (Hashtbl.find_all h n)) Lg.empty in
		  (Face.union f_id (inner edges_p), Face.union f_id (outer edges_p)) in    
		({i = i_c; o = e.o; p = p_c},
		 {i = e.i; o = o_d; p = p_d},
		 {i = f_id; o = f_id; p = Ports.empty})
		 ) t_a in					
  (* Build link graphs by removing empty edges*)
  let (u_c, u_d, id) =
    Array.fold_left (fun (acc_c, acc_d, acc_id) (c, d, id) ->
	    let aux e acc =
		    if (Face.is_empty e.i) && (Face.is_empty e.o) && (Ports.is_empty e.p)
		    then acc
			  else Lg.add e acc	in
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
(*
(* no extra node for hyperedges with only 2 ports [constraint=false]; *)
let toDot l =
	let (is, rest) = Lg.partition (fun (Edg (i, o, p)) ->
						Ports.is_empty p && Face.cardinal i = 1 && Face.cardinal o = 1) l
	in let (es, hs) = Lg.partition (fun (Edg (i, o, p)) ->
						Ports.cardinal p = 2 && Face.is_empty i && Face.is_empty o) rest
	(*|	let (hs, rest) = Lg.partition (fun (Edg (i, o, p)) ->                                    *)
	(*|						(Ports.cardinal p != 2 &&	not (Ports.is_empty p))                             *)
	(*|						|| Face.cardinal i > 1 || Face.cardinal o > 1) l                               *)
	(*|	in let (is, es) = Lg.partition (fun (Edg (i, o, p)) ->                                   *)
	(*|						Ports.is_empty p && Face.cardinal i = 1 && Face.cardinal o = 1) rest 					*)
	in let a =
		Lg.fold (fun (Edg (i, o, p)) buff ->
				(* print_endline "Link.toDot identies"; identities *)
						let x = Face.choose i
						and y = Face.choose o
						in buff ^ "\nO" ^ (string_of_name y) ^ " -> I" ^ (string_of_name x) ^ ";"
			) is ""
	and c =
		Lg.fold (fun (Edg (_, _, p)) buff ->
				(* print_endline "Link.toDot edges"; p has two elements *)
						let p0 = Ports.choose p
						in let p1 = Ports.choose (Ports.remove p0 p)
						in let (x, _) = p0
						and (y, _) = p1
						in buff ^ "\nv" ^ (string_of_int x) ^ " -> v" ^ (string_of_int y) ^ ";"
			) (Lg.filter (fun (Edg (_, _, p)) -> not (Ports.is_empty p)) es) ""
	and (_, b) =
		(* print_endline "Link.toDot hyperedges"; *)
		Lg.fold (fun (Edg (i, o, p)) (x, buff) ->
						match (Ports.cardinal p, Face.cardinal i, Face.cardinal o) with
						| (1, 1, 0) -> begin
									let (xp, _) = Ports.choose p
									and xi = string_of_name (Face.choose i)
									in (x, buff ^ "\nv" ^ (string_of_int xp)^ " -> I" ^ xi ^ ";")
								end
						| (1, 0, 1) -> begin
									let (xp, _) = Ports.choose p
									and xo = string_of_name (Face.choose o)
									in (x, buff ^ "\nO" ^ xo ^ " -> v" ^ (string_of_int xp) ^ ";")
								end
						| _ -> begin
									let j = string_of_int x
									in let lon = (*links from outer names to node*)
										Face.fold (fun (Nam x) buff ->
														buff ^ "\nO" ^ x ^ " -> e" ^ j ^ ";") o ""
									and lnv = (*liks from node to vertices*)
										Ports.fold (fun (x, _) buff ->
														buff ^ "\ne" ^ j ^ " -> v" ^ (string_of_int x) ^ ";") p ""
									and lni = (*links from node to inner names*)
										Face.fold (fun (Nam x) buff ->
														buff ^ "\ne" ^ j ^ " -> I" ^ x ^ ";") i ""
									in let n_buff = buff ^ "\ne" ^ j ^ " [shape=point, label=\"\", width=.001, height=.001, style=filled, color=green];" ^
										lon ^ lnv ^ lni
									in (x + 1, n_buff)
								end
			) hs (0, "")
	in a ^ b ^ c
*)

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
  
