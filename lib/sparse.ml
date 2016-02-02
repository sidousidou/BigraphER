let int_compare a b = a - b
       
module M_int =
  Map.Make (struct
	       type t = int
	       let compare = int_compare
	     end)

type bmatrix =
  { r: int;
    c: int;
    r_major: IntSet.t M_int.t; (* Row-major order: i -> j,...,j' *)
    c_major: IntSet.t M_int.t; (* Column-major order: j -> i,...,i' *)
  }

(* Create an empty matrix *)
let make r c =
  assert (r >= 0);
  assert (c >= 0);
  { r;
    c;
    r_major = M_int.empty;
    c_major = M_int.empty;
  }

let equal a b =
  a.r = b.r && a.c = b.c &&
    M_int.equal IntSet.equal a.r_major b.r_major

let compare a b =
  match a.r - b.r with
  | 0 -> (match a.c - b.c with
	  | 0 -> (match M_int.compare IntSet.compare a.r_major b.r_major with
		  | 0 -> M_int.compare IntSet.compare a.c_major b.c_major
		  | x -> x)
	  | x -> x)
  | x -> x

let to_string m =
  let buff = Array.make_matrix m.r m.c "0" in
  M_int.iter (fun i js ->
	      IntSet.iter (fun j ->
			   buff.(i).(j) <- "1")
			  js)
	     m.r_major;
  buff
  |> Array.map (fun r ->
		String.concat "" (Array.to_list r))
  |> Array.to_list 
  |> String.concat "\n"

let add_m i j m =
  try
    let js = M_int.find i m in
    M_int.add i (IntSet.add j js) m
  with
  | Not_found -> M_int.add i (IntSet.singleton j) m
  		   
let add i j m =
  assert (i >= 0);
  assert (j >= 0);
  assert (i < m.r);
  assert (j < m.c);
  { m with r_major = add_m i j m.r_major;
	   c_major = add_m j i m.c_major;
  }

let flip_major m =
  M_int.fold (fun i js acc ->
	      IntSet.fold (fun j acc ->
			   add_m j i acc)
			  js acc)
	     m M_int.empty
    
let off o_r o_c m =
  M_int.fold (fun i js acc ->
	      M_int.add (i + o_r) (IntSet.off o_c js) acc)
	     m M_int.empty

let merge_f _ l r =
  match (l, r) with
  | (Some s, Some s') -> Some (IntSet.union s s')
  | (Some s, None) | (None, Some s) -> Some s
  | (None, None) -> None
    
let row n =
  assert (n >= 0);
  let indexes = IntSet.of_int n in
  IntSet.fold (fun j acc -> add 0 j acc) indexes (make 1 n)
  
let col n =
  assert (n >= 0);
  let indexes = IntSet.of_int n in
  IntSet.fold (fun i acc -> add i 0 acc) indexes (make n 1)

let diag n =
  assert (n >= 0);
  let indexes = IntSet.of_int n in
  IntSet.fold (fun i acc -> add i i acc) indexes (make n n)
  
let tens a b =  
  { r = a.r + b.r;
    c = a.c + b.c;
    r_major = M_int.merge merge_f a.r_major (off a.r a.c b.r_major);
    c_major = M_int.merge merge_f a.c_major (off a.c a.r b.c_major);
  }
    
let append (a : bmatrix) (b : bmatrix) =
  assert (a.r = b.r);
  { r = a.r;
    c = a.c + b.c;
    r_major = M_int.merge merge_f a.r_major (off 0 a.c b.r_major);
    c_major = M_int.merge merge_f a.c_major (off a.c 0 b.c_major);
  }
    
let stack a b =
  assert (a.c = b.c);
  { r = a.r + b.r;
    c = a.c;
    r_major = M_int.merge merge_f a.r_major (off a.r 0 b.r_major);
    c_major = M_int.merge merge_f a.c_major (off 0 a.r b.c_major);
  }

let apply_rows_exn iso m =
  assert ((Iso.cardinal iso) = m.r);
  let (r_major, c_major) =
    M_int.fold (fun i js (acc_r, acc_c) ->
		let i' = Iso.apply_exn iso i in
		(M_int.add i' js acc_r,
		 IntSet.fold (fun j acc ->
			      M_int.add j (IntSet.singleton i') acc) js acc_c))
	       m.r_major (M_int.empty, M_int.empty) in
  { m with r_major;
	   c_major;
  }

let apply_cols_exn iso m =
  assert ((Iso.cardinal iso) = m.c);
  let (r_major, c_major) =
    M_int.fold (fun j is (acc_r, acc_c) ->
		let j' = Iso.apply_exn iso j in
		(IntSet.fold (fun i acc ->
			      M_int.add i (IntSet.singleton j') acc) is acc_r,
		 M_int.add j' is acc_c))
	       m.c_major (M_int.empty, M_int.empty) in
  { m with r_major;
	   c_major;
  }

let apply_exn iso m =
  assert ((Iso.cardinal iso) = m.r);
  assert (m.r = m.c);
  apply_rows_exn iso m
  |> apply_cols_exn iso
  
let parse_vectors adj rows =
  assert (rows >= 0);
  List.fold_left (fun (j, acc) i_list ->
		  (j + 1,
		   List.fold_left (fun acc i -> add i j acc) acc i_list))
		 (0, make rows (List.length adj)) adj
  |> snd

let chl m i =
  assert (i >= 0);
  assert (i < m.r);
  try M_int.find i m.r_major with
  | Not_found -> IntSet.empty

let prn m j =
  assert (j >= 0);
  assert (j < m.c);
  try M_int.find j m.c_major with
  | Not_found -> IntSet.empty
		   
let mul a b =
  assert (a.c = b.r);
  let m = make a.r b.c in 
  M_int.fold (fun i js acc ->
	      M_int.fold (fun j is acc ->
			  if IntSet.is_empty (IntSet.inter js is) then acc
			  else add i j acc)
			 b.c_major acc)
	     a.r_major m

let sum a b =
  assert (a.r = b.r);
  assert (a.c = b.c);
  M_int.fold (fun i js acc ->
	      IntSet.fold (fun j acc ->	add i j acc) js acc)
	     b.r_major a
  
let trans m0 =
  let rec fix m acc =
    let m' = mul m0 m in
    if equal m m' then acc 
    else fix m' (sum m' acc) in
  fix m0 m0

let dom m =
  M_int.bindings m.r_major
  |> List.split 
  |> fst
  |> IntSet.of_list
       
let codom m =
  M_int.bindings m.c_major
  |> List.split 
  |> fst
  |> IntSet.of_list
 
let leaves m =
  IntSet.diff (IntSet.of_int m.r) (dom m)
  
let orphans m =
  IntSet.diff (IntSet.of_int m.c) (codom m)

let siblings m j = 
  IntSet.fold (fun i acc ->
	       IntSet.union acc (chl m i)) 
	      (prn m j) IntSet.empty
  |> IntSet.remove j
		
(* let siblings_chk m = *)
(*   IntSet.for_all (fun j -> *)
(* 		  IntSet.is_empty (siblings m j)) *)
(* 		 (codom m) *)
		 
let partners m i =
  IntSet.fold (fun j acc ->
	       IntSet.union acc (prn m j))
	      (chl m i) IntSet.empty
  |> IntSet.remove i 

(* let partners_chk m = *)
(*   IntSet.for_all (fun i -> *)
(* 		  IntSet.is_empty (partners m i)) *)
(* 		 (dom m) *)

let iter f m = 
  M_int.iter (fun i js ->
	      IntSet.iter (fun j -> f i j) js)
	     m.r_major

let fold f m acc = 
  M_int.fold (fun i js acc ->
	      IntSet.fold (fun j acc -> f i j acc) js acc)
	     m.r_major acc

let add_list m =
  List.fold_left (fun acc (i, j) -> add i j acc) m

let entries m =
  M_int.fold (fun _ js acc ->
	      acc + (IntSet.cardinal js))
	     m.r_major 0

let levels m = 
  (* m is a graph *)
  assert (m.r = m.c);
  let rec fix acc nodes res =
    (* find nodes with all children in acc *)
    let leaves = 
      IntSet.filter (fun i ->
		     IntSet.subset (chl m i) acc)
		    nodes in
    if IntSet.is_empty leaves then res
    else
      fix (IntSet.union acc leaves) (IntSet.diff nodes leaves) 
	  (leaves :: res) in
  fix IntSet.empty (IntSet.of_int m.r) []

 let aux_split r m =     
     let (t, mi, b) = M_int.split r m in
     match mi with
     | Some js -> (t, M_int.add r js b)
     | None -> (t, b)

(* Dual of stack *)		 
let split_rows r m =
  assert (r > 0);
  assert (r <= m.r);
  let (top, b) = aux_split r m.r_major in
  let bottom = off (-r) 0 b in
  ({ r;
     c = m.c;
     r_major = top;
     c_major = flip_major top; 
   },
   { r = m.r - r;
     c = m.c;
     r_major = bottom;
     c_major = flip_major bottom;
   })

(* Dual of append *)    
let split_columns c m =
  assert (c > 0);
  assert (c <= m.c);
  let (left, right') = aux_split c m.c_major in
  let right = off (-c) 0 right' in
  ({ r = m.r;
     c;
     r_major = flip_major left;
     c_major = left; 
   },
   { r = m.r;
     c = m.c - c;
     r_major = flip_major right;
     c_major = right;
   })

let split r c m =
  let (t, b) = split_rows r m in
  (split_columns c t, split_columns c b)

(* Dual of split *)    
let glue rn rs nn ns =
  stack (append rn rs) (append nn ns)
    
exception PARSE_ERROR
    
let parse_string r n s rows =
  assert (r >= 0);
  assert (n >= 0);
  assert (s >= 0);
  assert (List.length rows = r + n);
  assert (List.for_all (fun l -> String.length l = n + s) rows);
  let to_int s =
    let rec aux i acc =
      if i < 0 then acc
      else match String.get s i with
	   | '1' -> aux (i - 1) (IntSet.add i acc)
	   | '0' -> aux (i - 1) acc
	   | _ -> raise PARSE_ERROR in
    aux ((String.length s) - 1) IntSet.empty in
  let (_, r_major) = List.fold_left (fun (i, acc) r ->
				     (i + 1, M_int.add i (to_int r) acc))
				    (0, M_int.empty) rows in
  { r = r + n;
    c = n + s;
    r_major;
    c_major = flip_major r_major;}
  |> split r n
