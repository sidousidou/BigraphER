type lit =
| M_lit of int * int    (* literal stored in a matrix *)
| V_lit of int          (* literal stored in a vector *)

type var = 
| P_var of lit (* Positive literal *)
| N_var of lit (* Negative literal *)

let to_M v i =
  match v with
  | V_lit j -> M_lit (i, j)
  | M_lit _ -> assert false

let to_N v =
  match v with
  | P_var l -> N_var l
  | N_var _ -> assert false

(* Convert a list of column vectors in a matrix. Each vector becomes a row. *)
let to_matrix l =
  fst (List.fold_left (fun (acc, i) row ->
    (acc @ (List.map (fun v -> to_M v i) row), i + 1)) ([], 0) l) 
  
(* Apply tseitin transformation to a bolean formula. Input is a list of pairs.
   Each pair encodes a conjunction. The first element of the output is a 
   disjunction of auxiliary variables. The second is a conjunctions of 
   (not z or a) (not z or b) ... *)
let tseitin l =
  fst (List.fold_left (fun ((acc_z, acc), i) ((a : var), (b : var)) ->
    let z = V_lit i in  
    (((P_var z) :: acc_z, (N_var z, a) :: (N_var z, b) :: acc),
     i + 1)) (([], []), 0) l)

let iff m clauses =
  (* a negated *)
  let pairs = List.map (fun a -> (m, to_N a)) (List.flatten clauses)
  (* m negated *)
  and l = List.map (fun c -> (to_N m) :: c) clauses in
  (pairs, l)

(* Commander variable encoding *)

type 'a cmd_tree = 
| Leaf of 'a list
| Node of ('a * 'a cmd_tree) list 

exception NO_GROUP

(* Return a list of groups of size at most g + 1.
   If [0;1;2] [3] then [0;1] [2;3] to avoid singletons. Also
   if [0;1] [2] then [0;1;2] *)
let group l n g =
  assert (g > 1);
  assert (n >= g);
  if List.length l <= n then raise NO_GROUP
  else let (x, i, res) = 
	 List.fold_left (fun (group, i, res)  x ->
	   if i >= g then ([x], 1, group :: res)
	   else (x :: group, i + 1, res)) ([], 0, []) l in
       if i = 1 then if g > 2 then
	   match res with
	   | (v :: vs) :: res -> (x @ [v]) :: vs :: res  
	   | _ -> assert false
	 else match res with
	 | v :: res -> (x @ v) :: res  
	 | _ -> assert false
       else x :: res 
  
(* Build a tree of commander variables. Input is a tree, output split the root 
   and add a level of variables. *)
let rec _cmd_init t n g j =
  match t with
  | Node cmd_l -> begin 
    try 
      let cmd_l' = group cmd_l n g  in
      let (j', t') = 
	List.fold_left (fun (j, acc) g -> 
	  (j + 1, (V_lit j, Node g) :: acc)) (j, []) cmd_l' in
      _cmd_init (Node t') n g j'
    with
    (* Do not add an additional level of commander variables *)
    | NO_GROUP -> t
  end
  | Leaf vars -> begin
    try 
      let cmd_l = group vars n g  in
      let (j', t') = 
	List.fold_left (fun (j, acc) g -> 
	  (j + 1, (V_lit j, Leaf g) :: acc)) (j, []) cmd_l in 
      _cmd_init (Node t') n g j'
    with
    (* Do not add an additional level of commander variables *)
    | NO_GROUP -> t
  end

let cmd_init (l : lit list) n g =
  _cmd_init (Leaf l) n g 0

(* Boolean encoding of at least one true. Most common cases are hardcoded *)
let rec _at_least l acc =
  match l with
  | [] | [_] -> acc
  | a :: [b] -> acc @ [(a, b)]
  | a :: b :: [c] -> acc @ [ (a, b); (a, c); (b, c) ]
  | a :: b :: c :: [d] -> acc @ [ (a, b); (a, c); (a, d); 
				  (b, c); (b, d); (c, d) ]
  | a :: b :: c :: d :: [e] -> acc @ [ (a, b); (a, c); (a, d); (a, e);
				       (b, c); (b, d); (b, e); (c, d);
				       (c, e); (d, e) ]
  | a :: b :: c :: d :: e :: [f] -> acc @ [ (a, b); (a, c); (a, d); (a, e); (a, f);
					    (b, c); (b, d); (b, e); (b, f); (c, d);
					    (c, e); (c, f); (d, e); (d, f); (e, f) ]
  | x :: rest -> _at_least rest (acc @ (List.map (fun y -> (x, y)) rest)) 

let at_least l =
 List.map (fun (a, b) -> (N_var a, N_var b)) ( _at_least l []) 
