(* Apply tseitin transformation to a bolean formula. Input is a list of pairs.
   Each pair encodes a conjunction. The first element of the output is a 
   disjunction of auxiliary variables. The second is a conjunctions of 
   (not z or a) (not z or b) ... *)
type m_var = int * int   (* variables stored in a matrix *)
type z_var = int         (* auxiliary variables stored in a vector *)

let tseitin l n =
     List.fold_left (fun ((acc_z, acc), (i : z_var)) ((a : m_var), (b : m_var)) ->
      ((i :: acc_z, (i, a) :: (i, b) :: acc), i + 1)) (([], []), n) l

let iff m clauses =
  (* a negated *)
  let pairs = List.map (fun a -> (m, a)) (List.flatten clauses)
  (* m negated *)
  and l = List.map (fun c -> m :: c) clauses in
  (pairs, l)


(* Commander variable encoding *)

type 'a cmd_tree = 
| Leaf of 'a list
| Node of ('a * 'a cmd_tree) list 

exception NO_GROUP

(* Return a list of groups of size aat most g + 1.
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
	  (j + 1, (j, Node g) :: acc)) (j, []) cmd_l' in (* j is a var *)
      aux (Node t') n g j'
    with
    (* Do not add an additional level of commander variables *)
    | NO_GROUP -> t
  end
  | Leaf vars -> begin
    try 
      let cmd_l = group vars n g  in
      let (j', t') = 
	List.fold_left (fun (j, acc) g -> 
	  (j + 1, (j, Leaf g) :: acc)) (j, []) cmd_l in (* j is a var *)
      aux (Node t') n g j'
    with
    (* Do not add an additional level of commander variables *)
    | NO_GROUP -> t
  end

let cmd_init l n g =
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
  _at_least l []  (* map to negated literals *)
