type bmatrix =
  { r: int;
    c: int;
    r_major: (int, int) Hashtbl.t; (* Row-major order: i -> j,...,j' *)
    c_major: (int, int) Hashtbl.t; (* Column-major order: j -> i,...,i' *)
  }

(* Create an empty matrix *)
let make rows cols =
  assert (rows >= 0);
  assert (cols >= 0);
  { r = rows;
    c = cols;
    r_major = Hashtbl.create rows;
    c_major = Hashtbl.create cols;
  }

let copy m =
  { m with
    r_major = Hashtbl.copy m.r_major;
    c_major = Hashtbl.copy m.c_major;
  }

let ( = ) a b = 
  (a.r = b.r) && (a.c = b.c) && 
    (a.r_major = b.r_major) && (a.c_major = b.c_major)

let compare a b =
  match a.r - b.r with
  | 0 ->
     (match a.c - b.c with
      | 0 ->
	 (match compare a.r_major b.r_major with
	  | 0 -> compare a.c_major b.c_major
	  | x -> x)
      | x -> x)
  | x -> x

let to_string m =
  let buff = Array.make_matrix m.r m.c "0" in
  Hashtbl.iter (fun i j -> 
		buff.(i).(j) <- "1") m.r_major;
  buff
  |> Array.map (fun r ->
		String.concat "" (Array.to_list r))
  |> Array.to_list 
  |> String.concat "\n"
		
let row n =
  assert (n >= 0);
  let m = make 1 n in (* inline to speed up *)
  for i = n - 1 downto 0 do (* fold returns the columns in ascending order *)
    (* 0 -> 0,...,n-1 *)
    Hashtbl.add m.r_major 0 i;
    (* 0,...,n-1 -> 0 *)
    Hashtbl.add m.c_major i 0
  done;
  m 

let col n =
  assert (n >= 0);
  let m = make n 1 in (* inline to speed up *)
  for i = n - 1 downto 0 do
    (* 0,...,n-1 -> 0 *)  
    Hashtbl.add m.r_major i 0;
    (* 0 -> 0,...,n-1 *)
    Hashtbl.add m.c_major 0 i 
  done;
  m 

let diag n =
  assert (n >= 0);
  let m = make n n in
  for i = 0 to n - 1 do
    Hashtbl.add m.r_major i i;
    Hashtbl.add m.c_major i i;
  done;
  m

let tens a b =
  let m = make (a.r + b.r) (a.c + b.c) in
  (* Insert elements of a *)
  Hashtbl.iter (fun i j -> 
		Hashtbl.add m.r_major i j;
		Hashtbl.add m.c_major j i)
	       a.r_major;
  (* Insert elements of b *)
  Hashtbl.iter (fun i j -> 
		Hashtbl.add m.r_major (i + a.r) (j + a.c);
		Hashtbl.add m.c_major (j + a.c) (i + a.r))
	       b.r_major;
  m
    
let append (a : bmatrix) (b : bmatrix) =
  assert (Pervasives.(=) a.r b.r);
  let m = make a.r (a.c + b.c) in
  (* Insert elements of a *)
  Hashtbl.iter (fun i j -> 
		Hashtbl.add m.r_major i j;
		Hashtbl.add m.c_major j i)
	       a.r_major;
  (* Insert elements of b *)
  Hashtbl.iter (fun i j -> 
		Hashtbl.add m.r_major i (j + a.c);
		Hashtbl.add m.c_major (j + a.c) i)
	       b.r_major;
  m
    
let stack a b =
  assert (Pervasives.(=)  a.c b.c);
  let m = make (a.r + b.r) a.c in
  (* Insert elements of a *)
  Hashtbl.iter (fun i j -> 
		Hashtbl.add m.r_major i j;
		Hashtbl.add m.c_major j i)
	       a.r_major;
  (* Insert elements of b *)
  Hashtbl.iter (fun i j -> 
		Hashtbl.add m.r_major (i + a.r) j;
		Hashtbl.add m.c_major j (i + a.r))
	       b.r_major;
  m

let apply_rows_exn iso m =
  assert (Pervasives.(=) (Iso.cardinal iso) m.r);
  let m' = make m.r m.c in
  Hashtbl.iter (fun i j ->
		let i' = Iso.apply_exn iso i in
		Hashtbl.add m'.r_major i' j;
		Hashtbl.add m'.c_major j i')
	       m.r_major;
  m'

let apply_cols_exn iso m =
  assert (Pervasives.(=) (Iso.cardinal iso) m.c);
  let m' = make m.r m.c in
  Hashtbl.iter (fun i j ->
		let j' = Iso.apply_exn iso j in
		Hashtbl.add m'.r_major i j';
		Hashtbl.add m'.c_major j' i)
	       m.r_major;
  m'

let apply_exn iso m =
  assert (Pervasives.(=) (Iso.cardinal iso) m.r);
  assert (Pervasives.(=) m.r m.c);
  let m' = make m.r m.c in
  Hashtbl.iter (fun i j ->
		let (i', j') = 
		  (Iso.apply_exn iso i, Iso.apply_exn iso j) in
		Hashtbl.add m'.r_major i' j';
		Hashtbl.add m'.c_major j' i')
	       m.r_major;
  m'

let add m i j =
  assert (i >= 0);
  assert (j >= 0);
  assert (i < m.r);
  assert (j < m.c);
  Hashtbl.add m.r_major i j;
  Hashtbl.add m.c_major j i

let parse_vectors adj rows =
  assert (rows >= 0);
  let m = make rows (List.length adj) in
  List.iteri (fun j i_list ->
	      List.iter (fun i -> add m i j) i_list)
	     adj;
  m
    
let chl m i =
  assert (i >= 0);
  assert (i < m.r);
  Hashtbl.find_all m.r_major i

let chl_set m i = IntSet.of_list (chl m i)		   

let prn m j =
  assert (j >= 0);
  assert (j < m.c);
  Hashtbl.find_all m.c_major j

let prn_set m j = IntSet.of_list (prn m j)
		   
let mul a b =
  assert (Pervasives.(=) a.c b.r);
  let m = make a.r b.c 
  and acc = Hashtbl.create a.r in
  Hashtbl.iter
    (fun i j ->
     let vec = Hashtbl.find_all b.r_major j in
     List.iter (fun k ->
		if List.mem k (Hashtbl.find_all acc i) then () 
		else 
		  (Hashtbl.add m.r_major i k;
		   Hashtbl.add m.c_major k i;
		   Hashtbl.add acc i k;))
	       vec) 
    a.r_major;
  m

(* It seems the same entries are checked too many times.
   Leaves don't need to be scanned. They can be removed and added again at the
   end of the recursion. *)
let trans m =
  let t = copy m in
  let rec fix () =
    let chl_2 = 
      Hashtbl.fold (fun i j acc ->
		    acc @ (List.map (fun c -> (i, c)) (chl m j))) 
		   t.r_major [] in
    let count =
      List.fold_left (fun acc (i, c) ->
		      if List.mem c (Hashtbl.find_all t.r_major i) then acc
		      else
			(Hashtbl.add t.r_major i c;
			 Hashtbl.add t.c_major c i;
			 acc + 1))
		     0 chl_2 in
    if count > 0 then fix ()
    else t in
  fix ()

let dom m =
  Hashtbl.fold (fun i _ acc ->
		IntSet.add i acc)
	       m.r_major IntSet.empty

let codom m =
  Hashtbl.fold (fun j _ acc ->
		IntSet.add j acc)
	       m.c_major IntSet.empty

let rec _iter i acc d =
  if i < 0 then acc
  else if IntSet.mem i d then _iter (i - 1) acc d
  else _iter (i - 1) (IntSet.add i acc) d
	       
let leaves m =
  _iter (m.r - 1) IntSet.empty (dom m)

let orphans m =
  _iter (m.c - 1) IntSet.empty (codom m)

let siblings m j = 
  prn m j
  |> List.fold_left (fun acc i ->
		     IntSet.union acc (IntSet.of_list (chl m i))) 
		    IntSet.empty
  |> IntSet.remove j
		
let siblings_chk m =
  IntSet.for_all (fun j ->
		  IntSet.is_empty (siblings m j))
		 (codom m)
		 
let partners m i =
  chl m i
  |> List.fold_left (fun acc j ->
		     IntSet.union acc (IntSet.of_list (prn m j)))
		    IntSet.empty
  |> IntSet.remove i 

let partners_chk m =
  IntSet.for_all (fun i ->
		  IntSet.is_empty (partners m i)) (dom m)

let iter f m = 
  Hashtbl.iter f m.r_major

let fold f m acc = 
  Hashtbl.fold f m.r_major acc

let add_list m =
  List.iter (fun (i, j) -> add m i j)

let entries m =
  Hashtbl.length m.r_major

let levels m = 
  (* m is a graph *)
  assert (Pervasives.(=) m.r m.c);
  let rec fix acc nodes res =
    (* find nodes with all children in acc *)
    let leaves = 
      IntSet.filter (fun i ->
		     IntSet.subset (IntSet.of_list (chl m i)) acc)
		    nodes in
    if IntSet.is_empty leaves then res
    else
      fix (IntSet.union acc leaves) (IntSet.diff nodes leaves) 
	  (leaves :: res) in
  fix IntSet.empty (IntSet.of_int m.r) []
