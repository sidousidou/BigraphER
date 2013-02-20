open Printf
open Bigarray

open Base

type bmatrix = (int, int8_unsigned_elt, c_layout) Array2.t
(*type bmatrix = {r: int; c: int; m: bool array array}*)

(* Create a rows x cols matrix *)
let make rows cols =
  Array2.create int8_unsigned c_layout rows cols

(* String representation. 0 = false and 1 = true. *)
let to_string m =
  let buff = Buffer.create (2 * (Array2.dim1 m) * (Array2.dim2 m)) in
  for i = 0 to (Array2.dim1 m) - 1 do  
    for j = 0 to (Array2.dim2 m) - 2 do
      Buffer.add_string buff (sprintf "%d " m.{i,j})      
    done;
    Buffer.add_string buff (sprintf "%d\n" m.{i,(Array2.dim2 m) - 1})
  done;
  match Buffer.length buff with
    | 0 -> ""
    | l -> Buffer.sub buff 0 (l - 1)

let row_1 n =
  let m = make 1 n in
  Array2.fill m 1;
  m
    
let row_0 n =
  let m = make 1 n in
  Array2.fill m 0;
  m

let col_1 n =
  let m = make n 1 in
  Array2.fill m 1;
  m

let col_0 n =
  let m = make n 1 in
  Array2.fill m 0;
  m

(* Create a diagonal matrix. true on the diagonal *)
let diag n =
  assert (n >= 0);
  let m = make n n in
  Array2.fill m 0;
  for i = 0 to n - 1 do
    m.{i,i} <- 1
  done;
  m

(* Stack two matrices with the same number of columns vertically 
   raise Assert_failure
   
              A
   A,B ---> -----
              B 
*)
let stack a b =
  assert (Array2.dim2 a = Array2.dim2 b);
  let m = make ((Array2.dim1 a) + (Array2.dim1 b)) (Array2.dim2 a) in
  Array2.blit a (Array2.sub_left m 0 (Array2.dim1 a));
  Array2.blit b (Array2.sub_left m (Array2.dim1 a) (Array2.dim1 b));
  m
 
(* Append two matrices with the same number of rows horizontally
   raise Assert_failure  
   
              |
   A,B ---> A | B
              | 
*)
let append a b =
  assert (Array2.dim1 a = Array2.dim1 b);
  let m = make (Array2.dim1 a) ((Array2.dim2 a) + (Array2.dim2 b)) in
  for i = 0 to (Array2.dim1 a) - 1 do
    for j = 0 to (Array2.dim2 a) - 1 do
      m.{i,j} <- a.{i,j}
    done;
    for j = (Array2.dim2 a) to (Array2.dim2 m) - 1 do
      m.{i,j} <- b.{i,j - (Array2.dim2 a)}
    done;
  done;
  m
  
(* Boolean matrix multiplication 
   raise Assert_failure   *)
let mul a b =
  assert (Array2.dim2 a = Array2.dim1 b);
  let m = make (Array2.dim1 a) (Array2.dim2 b) in
  for i = 0 to (Array2.dim1 a) - 1 do
    for j = 0 to (Array2.dim2 b) - 1 do
      for k = 0 to (Array2.dim1 b) - 1 do
        m.{i,j} <- if m.{i,j} = 1 then 1 else a.{i,k} * b.{k,j}
      done;
    done;
  done;
  m

(*    
(* Copy matrix a *)
let copy a =
  let copy_matrix m r =
    Array.init r (fun i -> Array.copy m.(i))
  in
  {r = a.r;
   c = a.c;
   m = copy_matrix a.m a.r}

(* Swap rows in place *)
let swap_rows a i j =
  let b = copy a
  in b.m.(i) <- b.m.(j);
  b.m.(j) <- Array.copy a.m.(i)
  
(* Swap cols in place *)
let swap_cols a i j =
  let b = copy a
  in Array.iteri (fun k r ->
    b.m.(k).(i) <- a.m.(k).(j);
    b.m.(k).(j) <- a.m.(k).(i)) b.m
*)

(* Split matrix a in four submatrices as follows

       H | K 
   A = -----     H = i x j
       C | D                          
   raise Assert_failure if arguments are invalid *)
let split m i j = 
  (* Dual of append. j is the second dimension of the left submatrix. *)
  let _split m j =
    let a = make (Array2.dim1 m) j
    and b = make (Array2.dim1 m) ((Array2.dim2 m) - j) in
    for i = 0 to (Array2.dim1 m) - 1 do
      for j_a = 0 to j - 1 do
	a.{i,j_a} <- m.{i,j_a}
      done;
      for j_b = 0 to (Array2.dim2 m) - j - 1 do
	b.{i,j_b} <- m.{i, j_b + j}
      done;
    done;
    a, b in
  assert (i >= 0 && j >= 0);
  assert (i <= (Array2.dim1 m) && j <= (Array2.dim2 m));
  let h, k = _split (Array2.sub_left m 0 i) j
  and c, d = _split (Array2.sub_left m i ((Array2.dim1 m) - i)) j in
  h, k, c, d

(* Apply isomorphism i to place graph m. The result is a fresh matrix. Argument
   r is the number of roots. Iso iso is assumed to be total (no check performed).
 *)
let apply_iso iso m r =
  let new_m = make (Array2.dim1 m) (Array2.dim2 m) 
  and n = (Array2.dim1 m) - r in
  (*let s = (Array2.dim2 m) - n in*)
  (* roots-nodes: permute columns *)
  for i = 0 to r - 1 do
    for j = 0 to n - 1 do
      new_m.{i,get_i j iso} <- m.{i,j}
    done;
  done;
  (* roots-sites: copy *)
  for i = 0 to r - 1 do
    for j = n to (Array2.dim2 m) - 1 do
      new_m.{i,j} <- m.{i,j}
    done;
  done;
  (* nodes-nodes: permute columns and rows *)
  for i = r to (Array2.dim1 m) - 1 do
    for j = 0 to n - 1 do
      new_m.{(get_i (i - r) iso) + r,get_i j iso} <- m.{i,j}
    done;
  done;
  (* nodes-sites: permute rows *)
  for i = r to (Array2.dim1 m) - 1 do
    for j = n to (Array2.dim2 m) - 1 do
      new_m.{(get_i (i - r) iso) + r,j} <- m.{i,j}
    done;
  done;
  new_m

(* Build a matrix starting from a list of parent (rows) sets *)
let parse_vector adj rows =
  let m = make rows (List.length adj) in 
  Array2.fill m 0;
  Array.iteri (fun j i_list ->
    List.iter (fun i -> 
      m.{i,j} <- 1) i_list) (Array.of_list adj);
  m 

(* Build a list of parent (rows) sets starting from a matrix *)
let get_vector m =
  let out = ref [] in 
  for j = (Array2.dim2 m) - 1 downto 0 do
    let p_set = ref [] in
    for i = (Array2.dim1 m) - 1 downto 0 do
      if m.{i,j} = 1 then p_set := i::!p_set else ()
    done;
    out := !p_set::!out
  done;    
  !out
                         
(* Get the set of children (columns) of an element i (row), 
   i.e. all the m.(i).(j) = true *)
let chl m i = 
  assert (i >=0 && i < Array2.dim1 m);
  let res = ref Int_set.empty in
  for j = 0 to (Array2.dim2 m) - 1 do
    if m.{i,j} = 1 then res := Int_set.add j !res else ()
  done;
  !res

(* Get the set of parents (rows) of an element j (column),
   i.e. all the m.(i).(j) = true *)
let prn m j = 
  assert (j >=0 && j < Array2.dim2 m);
  let res = ref Int_set.empty in
  for i = 0 to (Array2.dim1 m) - 1 do
    if m.{i,j} = 1 then res := Int_set.add i !res else ()
  done;
  !res

(* Get empty rows (leaves) *)
let zero_rows m =
  let res = ref Int_set.empty in
  for i = 0 to (Array2.dim1 m) - 1 do
    if Int_set.is_empty (chl m i) then res := Int_set.add i !res else ()
  done;
  !res

(* Get empty cols (orphans) *)
let zero_cols m =
  let res = ref Int_set.empty in
  for j = 0 to (Array2.dim2 m) - 1 do
    if Int_set.is_empty (prn m j) then res := Int_set.add j !res else ()
  done;
  !res

(* Warshall algorithm. *)
(* m is assumed square. *)
let trans m =
  let res = make (Array2.dim1 m) (Array2.dim1 m) in
  Array2.blit m res;
  for k = 0 to (Array2.dim1 res) - 1 do
    for i = 0 to (Array2.dim1 res) - 1 do
      for j = 0 to (Array2.dim1 res) - 1 do
        res.{i,j} <- if res.{i,j} = 1 then 1 else res.{i,k} * res.{k,j}
      done;
    done;
  done;
  (*printf "Matrix.trans\n%s\n\n%s\n" (to_string m) (to_string res);*)
  res

let trans_ref m = 
  let res = trans m in
  for i = 0 to (Array2.dim1 res) - 1 do
    res.{i,i} <- 1
  done;
  m  

let to_iso m = 
  let res = ref Iso.empty in
  for i = 0 to (Array2.dim1 m) - 1 do
    for j =0 to (Array2.dim2 m) - 1 do
      if m.{i,j} == 1 then res := Iso.add (i, j) !res else ()
    done;
  done;
  !res

(*
(* DEBUG *)
let _ = 
  let a = diag 5
  and b = diag 3 in
  printf "a[%d,%d]:\n%s\nb[%d,%d]:\n%s\n" 5 5 (to_string a) 3 3 (to_string b);
  printf "stack a a:\n%s\n" (to_string (stack a a));
  let c = diag 0
  and d = diag 1 in
  printf "c[%d,%d]:\n%s\nd[%d,%d]:\n%s\n" 0 0 (to_string c) 1 1 (to_string d);
  printf "stack c c:\n%s\n" (to_string (stack c c));
  printf "stack d d:\n%s\n" (to_string (stack d d));
  printf "append a a:\n%s\n" (to_string (append a a)); 
  printf "append c c:\n%s\n" (to_string (append c c));
  printf "append d d:\n%s\n" (to_string (append d d));
  printf "mul b [1 1 1]:\n%s\n" (to_string (mul b (stack d (stack d d))));
  printf "mul a a:\n%s\n" (to_string (mul a a));
  let h0, h1, k0, k1 = split a 2 3  in
  printf "h0:\n%s\nh1:\n%s\nk0:\n%s\nk1:\n%s\n" 
    (to_string h0) (to_string h1) (to_string k0) (to_string k1);
  printf "mul h0 k1:\n%s\n" (to_string (mul h0 k1));
  let m0, m1, n0, n1 = split k1 0 1 in
  printf "m0:\n%s\nm1:\n%s\nn0:\n%s\nn1:\n%s\n" 
    (to_string m0) (to_string m1) (to_string n0) (to_string n1);
  let v0 = parse_vector [[0;2]; [2;3]; [0;3;2]] 4
  and v1 = get_vector k1 in
  printf "v0:\n%s\n" (to_string v0);
  printf "v1:\n[%s]\n" (String.concat "; "(List.map (fun l ->
    sprintf "[%s]" (String.concat ";" (List.map string_of_int l))) v1));
  let iso = of_list [(0,1); (1,2); (2,0)] 
  and i0 = parse_vector [[0;1]; [1]; []] 2
  and i1 = parse_vector [[]] 2
  and i2 = parse_vector [[]; []; [0]] 3
  and i3 = parse_vector [[2]] 3 in
  let i = stack (append i0 i1) (append i2 i3) in
  printf "i:\n%s\niso(i):\n%s\n" (to_string i) (to_string (apply_iso iso i 2));
  printf "chl(1): %s\nchl(3): %s\nchl(4): %s\n" (string_of_Int_set (chl i 1)) 
    (string_of_Int_set (chl i 3))  (string_of_Int_set (chl i 4));
  printf "prn(3): %s\nprn(1): %s\nprn(0): %s\n" (string_of_Int_set (prn i 3)) 
    (string_of_Int_set (prn i 1))  (string_of_Int_set (prn i 0));
  printf "leaves(i): %s\noprhans(i): %s\n" (string_of_Int_set (zero_rows i))
    (string_of_Int_set (zero_cols i))
  *)
  
