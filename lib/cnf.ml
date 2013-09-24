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

  
  
