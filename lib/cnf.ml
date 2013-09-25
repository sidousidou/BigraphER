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

(* Convert a list of column vectors to a list of matrix inidices. Each vector becomes a row. *)
let to_matrix l =
  fst (List.fold_left (fun (acc, i) row ->
    (acc @ (List.map (fun v -> to_M v i) row), i + 1)) ([], 0) l) 
  
(* Apply tseitin transformation to a boolean formula. Input is a list of pairs.
   Each pair encodes a conjunction. The first element of the output is a 
   disjunction of auxiliary variables. The second is a conjunctions of 
   (not z or a) (not z or b) ... *)
let tseitin l =
  fst (List.fold_left (fun ((acc_z, acc), i) ((a : lit), (b : lit)) ->
    let z = V_lit i in  
    (((P_var z) :: acc_z, (N_var z, P_var a) :: (N_var z, P_var b) :: acc),
     i + 1)) (([], []), 0) l)

let iff (m : lit) (clauses : lit list list) =
  (* a negated *)
  let pairs = List.map (fun a -> (P_var m, N_var a)) (List.flatten clauses)
  (* m negated *)
  and l = List.map (fun c -> 
    (N_var m) :: (List.map (fun v -> P_var v) c)) clauses in
  (pairs, l)

(* +++++++++++++++++ Commander variable encoding +++++++++++++++++ *)

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

(* Number of auxiliary variables *)
let cmd_size t =
  match t with
  | Leaf _ -> 0
  | Node cmd_g -> begin
    match fst (List.hd cmd_g) with
    | V_lit n -> n + 1
    | _ -> assert false
  end

(* Boolean encoding of at most one TRUE. Most common cases are hardcoded *)
let rec _at_most l acc =
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
  | x :: rest -> _at_most rest (acc @ (List.map (fun y -> (x, y)) rest)) 

(* Disjuntions (all possible pairs) of negative literals *)
let at_most l =
 List.map (fun (a, b) -> (N_var a, N_var b)) ( _at_most l []) 

(* Disjunction of positive literals *)
let at_least l =
  List.map (fun x -> P_var x) l

(* Scan the tree and produce constraints:
   1. at most one TRUE in every group
   2. if commander variable is TRUE then at least one TRUE in its group
   3. if commander variable is FALSE then no TRUE in its group 
   4. exactly one commander variable is true. *)

(* [X0, X1, X2] -> [(!X0 or !X1), (!X0 or !X2), (!X1 or !X2)] *)
let rec _scan1 t acc =
  match t with 
  | Leaf g -> acc @ (at_most g)
  | Node cmd_g -> 
    let (cmd_vars, sub) = List.split cmd_g in
    acc @ (at_most cmd_vars) @
      (List.fold_left (fun acc t ->
	acc @ (_scan1 t [])) [] sub) 

(* (C, [X0, X1, X2]) -> [!C or X0 or X1 or X2] *)		  
let rec _scan2 t (acc : var list list) : (lit list * var list list) =
  match t with
  | Leaf g -> (g, acc)
  | Node cmd_g ->
    let cmd_vars = fst (List.split cmd_g)
    and acc' = List.fold_left (fun res (cmd_v, sub) ->
      let (g, acc) = _scan2 sub [] in
      let clause =
	(N_var cmd_v) :: (List.map (fun l -> P_var l) g) in	
      res @ (clause :: acc)) [] cmd_g in
    (cmd_vars, acc @ acc')

(* (C, [X0, X1, X2]) -> [(C or !X0), (C or !X1), (C or !X2)] *)		  
let rec _scan3 t acc : (lit list * (var * var) list) =
    match t with
  | Leaf g -> (g, acc)
  | Node cmd_g ->
    let cmd_vars = fst (List.split cmd_g)
    and acc' = List.fold_left (fun res (cmd_v, sub) ->
      let (g, acc) = _scan3 sub [] in
      let clause =
	List.map (fun l -> (P_var cmd_v, N_var l)) g in	
      res @ clause @ acc) [] cmd_g in
    (cmd_vars, acc @ acc')

let at_most_cmd t =
  let clauses1 = _scan1 t []
  and (_, clauses2) = _scan2 t []
  and (_, clauses3) = _scan3 t [] in
  (clauses1, clauses2, clauses3)

let at_least_cmd t =
  match t with
  | Leaf g -> at_least g
  | Node cmd_g -> at_least (fst (List.split cmd_g))

let exactly_one_cmd t =
  let (clauses1, clauses2, clauses3) = at_most_cmd t in
  (clauses1, clauses2, clauses3, at_least_cmd t)

let t_debug =
  Node
    [(V_lit 7,
      Node
	[(V_lit 3, Leaf [M_lit (0, 8); M_lit (0, 7); M_lit (0, 6)]);
	 (V_lit 4, Leaf [M_lit (0, 5); M_lit (0, 4); M_lit (0, 3)]);
	 (V_lit 5, Leaf [M_lit (0, 2); M_lit (0, 1); M_lit (0, 0)])]);
     (V_lit 6,
      Node
	[(V_lit 0, Leaf [M_lit (0, 15); M_lit (0, 14)]);
     (V_lit 1, Leaf [M_lit (0, 13); M_lit (0, 12)]);
     (V_lit 2, Leaf [M_lit (0, 11); M_lit (0, 10); M_lit (0, 9)])])]


(* +++++++++++++++++ Higher level functions +++++++++++++++++ *)

(* n size *)
let rec _iter f res i n =
  if i < n then begin
    let res' = f i res in
    _iter f res' (i + 1) n
  end else res
    
let iter f res n =
  _iter f res 0 n

(* Generate constraints for a bijection from n to m. Parameters t and g
   are used for configure the commander-variable encoding. The function can 
   be split into two parts:
    1. exactly one TRUE in every row of the assignements matrix
    2. at most one TRUE in every column of the assignments matrix.
   Auxiliary variables are returned. *)
let bijection n m t g =
  assert (m > 0);
  assert (n > 0);
  (* Rows *)
  let res_rows =
    iter (fun i acc ->
      let row_i = iter (fun j acc ->
	(M_lit (i, j)) :: acc) [] m in
      let t = cmd_init row_i t g in
      (cmd_size t, exactly_one_cmd t) :: acc) [] n
  (* Columns *)
  and res_cols =
    iter (fun j acc ->
      let col_j = iter (fun i acc ->
      (M_lit (i, j)) :: acc) [] n in
      let t = cmd_init col_j t g in
      (cmd_size t, at_most_cmd t) :: acc) [] m in
  (List.rev res_rows, List.rev res_cols)
