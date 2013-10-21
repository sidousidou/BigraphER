(******************************************************************************)
(*                                                                            *)
(*        ______ __                          __     _______ ______            *)
(*       |   __ \__|.-----.----.---.-.-----.|  |--.|    ___|   __ \           *)
(*       |   __ <  ||  _  |   _|  _  |  _  ||     ||    ___|      <           *)
(*       |______/__||___  |__| |___._|   __||__|__||_______|___|__|           *)
(*                  |_____|          |__|                                     *)
(*                                                                            *)
(*       Bigraph Evaluator & Rewriting                                        *)
(*                                                                            *)
(*                                                                            *)
(*     Copyright (c) 2010-2013, Michele Sevegnani - University of Glasgow     *)       
(*                                                                            *)
(******************************************************************************)

type lit =
| M_lit of int * int    (* literal stored in a matrix *)
| V_lit of int          (* literal stored in a vector *)

type var = 
| P_var of lit (* Positive literal *)
| N_var of lit (* Negative literal *)

type clause = var list

type b_clause = var * var

(* Only positive variables *)
let to_ij v =
  match v with
  | P_var l ->
    begin match l with
    | M_lit (i, j) -> (i, j)
    | V_lit _ -> assert false
    end
  | N_var _ -> assert false

let string_of_lit l =
  match l with
  | M_lit (i, j) -> Printf.sprintf "(%d,%d)" i j
  | V_lit i -> Printf.sprintf "(%d)" i
    
let string_of_var v =
  match v with
  | P_var l -> string_of_lit l
  | N_var l -> "!" ^ (string_of_lit l)
    
let string_of_clause c = 
  "[" ^ (String.concat " V " (List.map string_of_var c)) ^  "]"

(* Conjunction of clauses *)
exception TSEITIN of clause list 
  
(* Apply tseitin transformation to a boolean formula. Input is a list of pairs.
   The encoding is not applied if the input list has length less than three.
   Cases with length three and four are hard-coded.
   Each pair encodes a conjunction. The first element of the output is a 
   disjunction of auxiliary variables. The second is a conjunctions of 
   (not z or a) (not z or b) ... *)
let tseitin l =
  match l with
  | [] -> raise (TSEITIN [ ])
  | [(x, y)] -> raise (TSEITIN [ [P_var x]; [P_var y] ])
  | [(x, y); (w, z)] -> raise (TSEITIN [ [P_var x; P_var w]; [P_var x; P_var z];
				         [P_var y; P_var w]; [P_var y; P_var z] ])
  | [(x, y); (w, z); (h, k)] -> 
    begin
      let (a0, a1, a2) = (V_lit 0, V_lit 1, V_lit 2) in
      ([ P_var a0; P_var a1; P_var a2 ],
       [ (N_var a0, P_var x); (N_var a0, P_var y); 
	 (N_var a1, P_var w); (N_var a1, P_var z); 
	 (N_var a2, P_var h); (N_var a2, P_var k) ]) 
    end
  | [(x, y); (w, z); (h, k); (u, v)] -> 
    begin
      let (a0, a1, a2, a3) = (V_lit 0, V_lit 1, V_lit 2, V_lit 3) in
      ([ P_var a0; P_var a1; P_var a2; P_var a3 ],
       [ (N_var a0, P_var x); (N_var a0, P_var y); 
	 (N_var a1, P_var w); (N_var a1, P_var z); 
	 (N_var a2, P_var h); (N_var a2, P_var k);
	 (N_var a3, P_var u); (N_var a3, P_var v)]) 
    end
  | _ -> fst (List.fold_left (fun ((acc_z, acc), i) ((a : lit), (b : lit)) ->
    let z = V_lit i in  
    (((P_var z) :: acc_z, (N_var z, P_var a) :: (N_var z, P_var b) :: acc),
     i + 1)) (([], []), 0) l)

let equiv (m : lit) (clauses : lit list list) =
  (* a negated *)
  let pairs = List.map (fun a -> (P_var m, N_var a)) (List.flatten clauses)
  (* m negated *)
  and l = List.map (fun c -> 
    (N_var m) :: (List.map (fun v -> P_var v) c)) clauses in
  (pairs, l)

let impl (m : lit) (clauses : lit list list) =
  (* m negated *)
  List.map (fun c -> 
    (N_var m) :: (List.map (fun v -> P_var v) c)
  ) clauses

let block_rows rows c =
  assert (c >= 0);
  let rec block_row i j acc =
    match j with
    | 0 -> [N_var (M_lit (i, 0))] :: acc
    | _ -> block_row i (j - 1) ([N_var (M_lit (i, j))] :: acc) in
  List.fold_left (fun acc i ->
    block_row i (c - 1) acc) [] rows

let blocking_pairs l = 
  List.map (fun (i, j) ->
    [N_var (M_lit (i, j))]) l

(* Input is a list of root commander variables *)
let block_cmd : int list -> clause list =
  List.map (fun i_z ->
    [N_var (V_lit i_z)])

(* ++++++++++++++++++++ Commander variable encoding ++++++++++++++++++++ *)

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
  | Node cmd_l -> 
    (try 
       let cmd_l' = group cmd_l n g  in
       let (j', t') = 
	 List.fold_left (fun (j, acc) g -> 
	   (j + 1, (V_lit j, Node g) :: acc)) (j, []) cmd_l' in
       _cmd_init (Node t') n g j'
     with
    (* Do not add an additional level of commander variables *)
     | NO_GROUP -> t)
  | Leaf vars -> 
    (try 
       let cmd_l = group vars n g  in
       let (j', t') = 
	 List.fold_left (fun (j, acc) g -> 
	   (j + 1, (V_lit j, Leaf g) :: acc)) (j, []) cmd_l in 
       _cmd_init (Node t') n g j'
     with
    (* Do not add an additional level of commander variables *)
     | NO_GROUP -> t)

(* M_lit elements *)
let cmd_init (l : lit list) n g =
  _cmd_init (Leaf l) n g 0

(* Number of auxiliary variables *)
let cmd_size t =
  match t with
  | Leaf _ -> 0
  | Node cmd_g -> 
    (match fst (List.hd cmd_g) with
    | V_lit n -> n + 1
    | M_lit _ -> assert false)
    
let cmd_roots t =
  match t with
  | Leaf _ -> []
  | Node cmd_g ->
    List.map (fun (root, _) ->
      match root with
      | V_lit i -> i
      | M_lit _ -> assert false
    ) cmd_g

(* Boolean encoding of at most one TRUE. Most common cases are hard-coded *)
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

(* Disjunctions (all possible pairs) of negative literals *)
let at_most l =
 List.map (fun (a, b) -> (N_var a, N_var b)) ( _at_most l []) 

(* Disjunction (clause) of positive literals *)
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

type cmd_constraint =
| Cmd_at_most of b_clause list * clause list * b_clause list
| Cmd_exactly of b_clause list * clause list * b_clause list * clause

let at_most_cmd t =
  let clauses1 = _scan1 t []
  and (_, clauses2) = _scan2 t []
  and (_, clauses3) = _scan3 t [] in
  Cmd_at_most (clauses1, clauses2, clauses3)

let at_least_cmd t =
  match t with
  | Leaf g -> at_least g
  | Node cmd_g -> at_least (fst (List.split cmd_g))

let exactly_one_cmd t =
  match at_most_cmd t with
  | Cmd_at_most (cl1, cl2, cl3) ->
    Cmd_exactly (cl1, cl2, cl3, at_least_cmd t)
  | Cmd_exactly _ -> assert false

(* let t_debug = *)
(*   Node *)
(*     [(V_lit 7, *)
(*       Node *)
(* 	[(V_lit 3, Leaf [M_lit (0, 8); M_lit (0, 7); M_lit (0, 6)]); *)
(* 	 (V_lit 4, Leaf [M_lit (0, 5); M_lit (0, 4); M_lit (0, 3)]); *)
(* 	 (V_lit 5, Leaf [M_lit (0, 2); M_lit (0, 1); M_lit (0, 0)])]); *)
(*      (V_lit 6, *)
(*       Node *)
(* 	[(V_lit 0, Leaf [M_lit (0, 15); M_lit (0, 14)]); *)
(*      (V_lit 1, Leaf [M_lit (0, 13); M_lit (0, 12)]); *)
(*      (V_lit 2, Leaf [M_lit (0, 11); M_lit (0, 10); M_lit (0, 9)])])] *)


(* ++++++++++++++++++++++++ Higher level functions ++++++++++++++++++++++++ *)

let rec _downto f acc i =
  if i >= 0  then (
    let acc' = f i acc in
    _downto f acc' (i - 1)
  ) else acc
    
let iter f acc n =
  _downto f acc n

type cmd = {
  length : int;                 (** Number of auxiliary commander variables *)
  roots : int list;             (** Root commander variables *)
  cmd : cmd_constraint array;   (** Constraints *) 
}

let _exactly_rows n m t g =
  let l = ref 0
  and r = ref [] in
  let c =
  Array.of_list (
    iter (fun i acc ->
      let row_i = iter (fun j acc ->
	(M_lit (i, j)) :: acc
      ) [] (m - 1) in
      let t = cmd_init row_i t g in
      if i = 0 then (
	l := cmd_size t;
	r := cmd_roots t);
      (exactly_one_cmd t) :: acc
    ) [] (n - 1)) in
  {length = !l; roots = !r; cmd = c}

(* Generate constraints for a bijection from n to m. Parameters t and g
   are used for configure the commander-variable encoding. The function can 
   be split into two parts:
    1. exactly one TRUE in every row of the assignments matrix
    2. at most one TRUE in every column of the assignments matrix.
   Auxiliary variables are returned. *)
let bijection n m t g =
  assert (m >= 0);
  assert (n >= 0);
  let l = ref 0
  and r = ref [] in
  let res_cols =
    Array.of_list (
      iter (fun j acc ->
	let col_j = 
	  iter (fun i acc ->
	    (M_lit (i, j)) :: acc
	  ) [] (n - 1) in
	let t = cmd_init col_j t g in
	if j = 0 then (
	  l := cmd_size t;
	  r := cmd_roots t);
	(at_most_cmd t) :: acc
      ) [] (m - 1)) in
  (_exactly_rows n m t g, {
    length = !l;
    roots = !r;
    cmd = res_cols}
  )
    
(* Generate constraints for a total, non-surjective function n to m. Parameters
   t and g  are used for configure the commander-variable encoding. The 
   function constructs the following constraint:
    -  exactly one TRUE in every row of the assignments matrix
   Auxiliary variables are returned. *)
let tot_fun n m t g =
  assert (m >= 0);
  assert (n >= 0);
  _exactly_rows n m t g

(* +++++++++++++++++++++++ Integration with Minisat +++++++++++++++++++++++ *)

(* Convert variables for Minisat *)
let convert_m v (m : Minisat.var array array) =
  let convert_lit l =
    match l with
    | M_lit (i, j) -> m.(i).(j)
    | V_lit _ -> assert false in
  match v with
  | P_var l -> Minisat.pos_lit (convert_lit l) 
  | N_var l -> Minisat.neg_lit (convert_lit l)

let convert_v v (vec : Minisat.var array) =
  let convert_lit l =
    match l with
    | V_lit i -> vec.(i)
    | M_lit _ -> assert false in
  match v with
  | P_var l -> Minisat.pos_lit (convert_lit l) 
  | N_var l -> Minisat.neg_lit (convert_lit l)

(* Convert to vector z if V_lit, to matrix m otherwise *)
let convert v (z : Minisat.var array) (m : Minisat.var array array) =
  let convert_lit l =
    match l with
    | V_lit i -> z.(i)
    | M_lit (i,j) -> m.(i).(j) in
  match v with
  | P_var l -> Minisat.pos_lit (convert_lit l) 
  | N_var l -> Minisat.neg_lit (convert_lit l)

(* Initialise a vector of (auxiliary) variables. *)
let init_aux_v n s =
  let v = Array.make n 0 in
  for i = 0 to n - 1 do
    v.(i) <- s#new_var
  done;
  v

(* Initialise a matrix of variables. *)
let init_aux_m r c s =
  let m = Array.make_matrix r c 0 in
  for i = 0 to r - 1 do
    for j = 0 to c - 1 do
      m.(i).(j) <- s#new_var
    done;
  done;
  m

(* Post conjunction of clauses to solver. All variables refer to the same
   vector. *)
let post_conj_v l s v =
  List.iter (fun clause ->
    s#add_clause (List.map (fun x ->
      convert_v x v) clause)) l

(* To be used also when TSEITIN is raised. All variables refer to the same
   matrix.*)
let post_conj_m l s m =
  List.iter (fun clause ->
    s#add_clause (List.map (fun x ->
      convert_m x m) clause)) l

(* Post Tseitin constraints to solver and return array of auxiliary 
   variables. *)
let post_tseitin (z_clause, pairs) s m =
  let z = init_aux_v (List.length z_clause) s in
  post_conj_v [z_clause] s z;
  List.iter (fun (a , v) ->
    s#add_clause [convert_v a z; convert_m v m]) pairs;
  z

(* Post impl constraints to solver. Left hand-sides are stored in matrix w. *)
let post_impl clauses s w v =
  List.iter (fun clause ->
    match clause with
    | z :: rhs ->
      let rhs' = 
	List.map (fun x -> convert_m x v) rhs in
      s#add_clause ((convert_m z w) :: rhs')
    | _ -> assert false
  ) clauses 

(* Post equiv constraints to solver. Left hand-sides are stored in matrix w. *)
let post_equiv (pairs, clauses) s w v =
  List.iter (fun (m, x) ->
    s#add_clause [convert_m m w; convert_m x v]) pairs;
  post_impl clauses s w v

(* V_lit are for auxiliary variables whereas M_lit are for encoding 
   variables. *)
let _post_pairs l s a v = 
  List.iter (fun (x, y) ->
    s#add_clause [ (convert x a v); (convert y a v) ]) l

let _post_list l s a v =
  List.iter (fun clause ->
  s#add_clause (List.map (fun x ->
    convert x a v) clause)) l

(* Post bijection constraints to solver and return two matrices of auxiliary
   variables. Root indices are the same for every row of the matrix. *)
let post_bij (r_cmd, c_cmd) s m =
  let aux_r =
    (Array.map (fun c ->
      match c with
      | Cmd_exactly (cl1, cl2, cl3, cl4) -> (
	let z = init_aux_v r_cmd.length s in
	_post_pairs cl1 s z m;
	_post_list cl2 s z m;
	_post_pairs cl3 s z m;
	_post_list [cl4] s z m;
	z)
      | Cmd_at_most _ -> assert false
     ) r_cmd.cmd,
     r_cmd.roots) 
  and aux_c =
    (Array.map (fun c ->
      match c with
      | Cmd_at_most (cl1, cl2, cl3) -> (
	let z = init_aux_v c_cmd.length s in
	_post_pairs cl1 s z m;
	_post_list cl2 s z m;
	_post_pairs cl3 s z m;
	z)
      | Cmd_exactly _ -> assert false
     ) c_cmd.cmd,
     c_cmd.roots) in
  (aux_r, aux_c)
    
(* Post total non-surjective function to solver and return auxiliary 
   variables. *)
let post_tot r_cmd s m =
  (Array.map (fun c ->
    match c with
    | Cmd_exactly (cl1, cl2, cl3, cl4) -> (
      let z = init_aux_v r_cmd.length s in
      _post_pairs cl1 s z m;
      _post_list cl2 s z m;
      _post_pairs cl3 s z m;
      _post_list [cl4] s z m;
      z)
    | Cmd_at_most _ -> assert false
   ) r_cmd.cmd,
   r_cmd.roots) 

(* Block a commander variable row *)
let post_block_cmd i s m roots =
  List.iter (fun r ->
    s#add_clause [Minisat.neg_lit m.(i).(r)]
  ) roots

let post_block j s m =
 Array.iteri (fun i _ ->
   s#add_clause [Minisat.neg_lit m.(i).(j)]
 ) m  
