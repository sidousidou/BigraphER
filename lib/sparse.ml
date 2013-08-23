type bmatrix =
  { r: int;
    c: int;
    r_major: (int, int) Hashtbl.t; (* Row-major order: i -> j,...,j' *)
    c_major: (int, int) Hashtbl.t; (* Column-major order: j -> i,...,i' *)
  }

(* Create an empty matrix *)
let make rows cols =
  assert(rows >= 0);
  assert(cols >= 0);
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

let to_string m =
  let buff = Array.make_matrix m.r m.c "0" in
  Hashtbl.iter (fun i j -> 
    buff.(i).(j) <- "1") m.r_major;
  String.concat "\n" (Array.to_list (Array.map (fun r ->
    String.concat "" (Array.to_list r)) buff))

let row_1 n =
  assert (n >= 0);
  let m = make 1 n in (* inline to speed up *)
  for i = n - 1 downto 0 do (* fold returns the columns in ascending order *)
    (* 0 -> 0,...,n-1 *)
    Hashtbl.add m.r_major 0 i;
    (* 0,...,n-1 -> 0 *)
    Hashtbl.add m.c_major i 0
  done;
  m 

let row_0 n =
  assert (n >= 0);
  make 1 n

let col_1 n =
  assert (n >= 0);
  let m = make n 1 in (* inline to speed up *)
  for i = n - 1 downto 0 do
    (* 0,...,n-1 -> 0 *)  
    Hashtbl.add m.r_major i 0;
    (* 0 -> 0,...,n-1 *)
    Hashtbl.add m.c_major 0 i 
  done;
  m 

let col_0 n =
  assert (n >= 0);
  make n 1

let diag n =
  assert (n >= 0);
  let m = make n n in
  for i =0 to n - 1 do
    Hashtbl.add m.r_major i i;
    Hashtbl.add m.c_major i i;
  done;
  m

let tens a b =
  let m = make (a.r + b.r) (a.c + b.c) in
  (* Insert elements of a *)
  Hashtbl.iter (fun i j -> 
    Hashtbl.add m.r_major i j) a.r_major;
  Hashtbl.iter (fun j i -> 
    Hashtbl.add m.c_major j i) a.c_major;
  (* Insert elements of b *)
  Hashtbl.iter (fun i j -> 
    Hashtbl.add m.r_major (i + a.r) (j + a.c)) b.r_major;
  Hashtbl.iter (fun j i -> 
    Hashtbl.add m.c_major (j + a.c) (i + a.r)) b.c_major;
  m

let append a b =
  assert (a.r = b.r);
  let m = make a.r (a.c + b.c) in
  (* Insert elements of a *)
  Hashtbl.iter (fun i j -> 
    Hashtbl.add m.r_major i j) a.r_major;
  Hashtbl.iter (fun j i -> 
    Hashtbl.add m.c_major j i) a.c_major;
  (* Insert elements of b *)
  Hashtbl.iter (fun i j -> 
    Hashtbl.add m.r_major i (j + a.c)) b.r_major;
  Hashtbl.iter (fun j i -> 
    Hashtbl.add m.c_major (j + a.c) i) b.c_major;
  m
  

let stack a b
  assert (a.c = b.c);
  let m = make (a.r + b.r) a.c in
  (* Insert elements of a *)
  Hashtbl.iter (fun i j -> 
    Hashtbl.add m.r_major i j) a.r_major;
  Hashtbl.iter (fun j i -> 
    Hashtbl.add m.c_major j i) a.c_major;
  (* Insert elements of b *)
  Hashtbl.iter (fun i j -> 
    Hashtbl.add m.r_major (i + a.r) j) b.r_major;
  Hashtbl.iter (fun j i -> 
    Hashtbl.add m.c_major j (i + a.r)) b.c_major;
  m

let apply_iso iso m =
  assert (Base.Iso.cardinal iso = m.r);
  assert (m.r = m.c);
  let m' = make m.r m.c in
  Hashtbl.iter (fun i j ->
    let (i', j') = 
      (Base.get_i i iso, Base.get_i j iso) in
    Hashtbl.add m'.r_major i' j') m.r_major;
  Hashtbl.iter (fun j i ->
    let (i', j') = 
      (Base.get_i i iso, Base.get_i j iso) in
    Hashtbl.add m'.c_major j' i') m.c_major;
  m'

let parse_vector adj rows =
  let m = make rows (List.length adj) in
  Array.iteri (fun j i_list ->
    List.iter (fun i -> 
      Hashtbl.add m.r_major i j;
      Hashtbl.add m.c_major j i) i_list) (Array.of_list adj);
  m
    
let chl m i =
  assert (i >= 0);
  assert (i < m.r);
  Hashtbl.find_all m.r_major i

let prn m j =
  assert (j >= 0);
  assert (j < m.c);
  Hashtbl.find_all m.c_major j

let zero_rows m =
  let rec iter i acc =
    if i < 0 then acc
    else match chl m i with
    | [] -> iter (i - 1) (i :: acc)
    | _ -> iter (i - 1) acc in
  iter (m.r - 1) []

let zero_cols m =
  let rec iter j acc =
    if j < 0 then acc
    else match prn m j with
    | [] -> iter (j - 1) (j :: acc)
    | _ -> iter (j - 1) acc in
  iter (m.c - 1) []

let siblings m j = 
  let p = prn m j in
  Base.Int_set.elements 
    (Base.Int_set.remove j (List.fold_left (fun acc i ->
      Base.Int_set.union acc (Base.set_of_list (chl m i)))
			      Base.Int_set.empty p))
  
let partners m i =
  let c = chl m i in
  Base.Int_set.elements 
    (Base.Int_set.remove i (List.fold_left (fun acc j ->
      Base.Int_set.union acc (Base.set_of_list (prn m j)))
			      Base.Int_set.empty c))

let mul a b =
  assert (a.c = b.r);
  let m = make a.r b.c in
  ignore (Hashtbl.fold (fun i j acc ->
    let vec = Hashtbl.find_all b.r_major j in
    List.fold_left (fun acc k ->
      if Base.Iso.mem (i, k) acc then acc
      else begin Hashtbl.add m.r_major i k;
	Hashtbl.add m.c_major k i;
	Base.Iso.add (i, k) acc
      end) acc vec) a.r_major Base.Iso.empty);
  m

(* It seems the same entries are checked too many times.
   Leaves don't need to be scanned. They can be removed and added again at the
   end of the recursion. *)
let trans m =
  let t = copy m in
  let rec fix () =
    let chl_2 = Hashtbl.fold (fun i j acc ->
      acc @ (List.map (fun c -> (i, c)) (chl m j))) t.r_major [] in
    let count = List.fold_left (fun acc (i, c) ->
    if List.mem c (Hashtbl.find_all t.r_major i) then acc
    else begin
      Hashtbl.add t.r_major i c;
      Hashtbl.add t.c_major c i;
      acc + 1
    end) 0 chl_2 in
    if count > 0 then fix ()
    else t in
  fix ()

(* Not exposed *)
let of_list l r c =
  let m = make r c in
  ignore (List.fold_left (fun i row ->
    ignore (List.fold_left (fun j x ->
      match x with
      | 0 -> j + 1
      | 1 -> begin 
	Hashtbl.add m.r_major i j;
	Hashtbl.add m.c_major j i;
	j + 1
      end) 0 row);
    i + 1) 0 l);
  m

(* DEBUG *)  
let () =
  let a = of_list [ [1;0;0];
		    [0;0;1];
		    [1;1;0];
		  ] 3 3
  and b = of_list [ [1;0];
		    [0;0];
		    [0;1];
		  ] 3 2 in
  print_endline (to_string a);
  print_newline ();
  print_endline (to_string b);
  print_newline ();
  print_endline (to_string (mul a b));
  print_newline ();
  print_endline (to_string (trans a));
  print_newline ();
  let c = of_list [ [1;0;1];
		    [1;0;0];
		    [0;1;1];
		    [1;0;0];
		  ] 4 3
  and d = of_list [ [1;1];
		    [0;1];
		    [1;1];
		  ] 3 2 in
  print_endline (to_string c);
  print_newline ();
  print_endline (to_string d);
  print_newline ();
  print_endline (to_string (mul c d));
  print_newline ();
  print_endline (to_string (trans c));
  print_newline ()



