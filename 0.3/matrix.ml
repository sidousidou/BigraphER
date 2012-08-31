open Printf

open Base

type bmatrix = {r: int; c: int; m: bool array array}

(* Create a rows x cols matrix with all its elements set to false *)
let make rows cols =
  {r = rows;
   c = cols;
   m = Array.make_matrix rows cols false}

(* Create a diagonal matrix. true on the diagonal *)
let diag n =
  let out  = make n n
  in Array.iteri (fun i r ->
    Array.iteri (fun j x ->
		if i = j then out.m.(i).(j) <- true else ()) r) out.m;
  out

(* Stack two matrices with the same number of columns vertically 
   raise Assert_failure *)
let stack a b =
  assert (a.c = b.c);
  {r = a.r + b.r; 
   c = a.c; 
   m = Array.init (a.r + b.r) (fun i -> 
       if i < a.r then
	      Array.copy a.m.(i)
       else
	      Array.copy b.m.(i - a.r))}
  
(* Append two matrices with the same number of rows horizontally
   raise Assert_failure  *)
let append a b =
  assert (a.r = b.r);
  {r = a.r; 
   c = a.c + b.c; 
   m = Array.init a.r (fun i -> Array.append a.m.(i) b.m.(i))}
  
(* Boolean matrix multiplication 
   raise Assert_failure   *)
let mul a b =
  let mul_row_col row col =
    List.fold_left2 (fun res x y -> res || (x && y)) false row col
  in
    assert (a.c = b.r);
    {r = a.r;
     c = b.c;
     m = Array.init a.r (fun i ->
       Array.init b.c (fun j -> mul_row_col (Array.to_list a.m.(i)) 
	   (Array.fold_left (fun acc r -> acc@[r.(j)]) [] b.m)))}

(* Copy matrix a *)
let copy a =
  let copy_matrix m r =
    Array.init r (fun i -> Array.copy m.(i))
  in
  {r = a.r;
   c = a.c;
   m = copy_matrix a.m a.r}

(* String representation. 0 = false and 1 = true *)
let to_string a =
  String.concat "\n" 
    (List.map (fun s -> String.concat " " 
        (List.map (fun x -> if x then "1" else "0") (Array.to_list s))) 
       (Array.to_list a.m))

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

(* Split matrix a in four submatrices as follows

       H | K 
   A = -----     H = i x j
       C | D                          
   raise Assert_failure if arguments are invalid *)
let split a i j = 
  assert (i >= 0 && j >= 0);
  assert (i <= a.r && j <= a.c);
  let u = Array.sub a.m 0 i
  and d = Array.sub a.m i (a.r - i)
  in let m_h = Array.map (fun r -> Array.sub r 0 j) u
  and m_k = Array.map (fun r -> Array.sub r j (a.c - j)) u
  and m_c = Array.map (fun r -> Array.sub r 0 j) d
  and m_d = Array.map (fun r -> Array.sub r j (a.c - j)) d
  in let h = {r = i; c = j; m = m_h}
  and k = {r = i; c = a.c - j; m = m_k}
  and c = {r = a.r - i; c = j; m = m_c}
  and d = {r = a.r - i; c = a.c - j; m = m_d}
  in (h, k, c, d)

(* Copy row a.(i) to b.(j) in place
   raise Assert_failure *)
let copy_row a b i j =
  assert (a.c = b.c);
  assert (i >= 0 && i < a.r);
  assert (j >= 0 && j < b.r);
  b.m.(j) <- Array.copy a.m.(i)

(* Copy column a.(_).(i) to b.(_).(j) in place 
   raise Assert_failure *)
let copy_col a b i j =
  assert (a.r = b.r);
  assert (i >= 0 && i < a.c);
  assert (j >= 0 && j < b.c);
  Array.iteri (fun k r ->
    b.m.(k).(j) <- a.m.(k).(i)) a.m

(* Apply isomorphism i to m. The result is a fresh matrix. Argument offset 
   is the number of roots. *)
let apply_iso i m offset =
  let fresh1 = copy m
  in
    (* Reorder columns *)
    Iso.iter (fun (x, y) ->
      copy_col m fresh1 x y) i;
    let fresh2 = copy fresh1
    in
      (* Reorder rows *)
      Iso.iter (fun (x, y) ->
        copy_row fresh1 fresh2 (x + offset) (y + offset)) i;
      fresh2  

(* Build a matrix starting from a list of parent (rows) sets *)
let parse_vector adj rows =
  let out = make rows (List.length adj)
  in Array.iteri (fun j i_list ->
    List.iter (fun i ->
      out.m.(i).(j) <- true) i_list) (Array.of_list adj);
  out    

(* Build a list of parent (rows) sets starting from a matrix *)
let get_vector m =
  let out = ref [] in 
  for j = m.c - 1 downto 0 do
    let p_set = ref [] in
    for i = m.r - 1 downto 0 do
      if m.m.(i).(j) then p_set := i::!p_set else ()
    done;
    out := !p_set::!out
  done;    
  !out

(* Get the set of children (columns) of an element i (row), 
   i.e. all the m.(i).(j) = true
   raise Assert_failure   *)
let chl m i = 
  assert (i >=0 && i < m.r);
  snd (Array.fold_left (fun (j, acc) b ->
    if b then
      (j + 1, Int_set.add j acc)
    else
      (j + 1, acc)) (0, Int_set.empty) m.m.(i))

(* Get the set of parents (rows) of an element j (column),
   i.e. all the m.(i).(j) = true
   raise Assert_failure   *)
let prn m j = 
  assert (j >=0 && j < m.c);
  snd (Array.fold_left (fun (i, acc) r ->
    if r.(j) then
      (i + 1, Int_set.add i acc)
    else
      (i + 1, acc)) (0, Int_set.empty) m.m)

(* DEBUG *)
(*let print m = 
  printf "%s\n" (to_string m) 

let a = 
  {r = 8;
   c = 7;
   m =
    [|[|true; true; false; false; false; false; false|];
      [|false; true; true; false; false; false; false|];
      [|false; false; true; false; false; false; true|];
      [|false; false; false; true; false; true; false|];
      [|false; false; false; true; true ; false; false|];
      [|false; false; false; true; false; false; true|];
      [|false; false; false; false; false; true; true|];
      [|false; false; false; false; false; false; false|];
      |]}


let b = 
  {r = 8;
   c = 7;
   m =
    [|[|false; false; true; true; false; false; false|];
      [|false; true; false; true; false; false; false|];
      [|false; true; false; false; false; false; true|];
      [|false; false; false; false; false; false; false|];
      [|false; false; false; false; true ; false; true|];
      [|false; false; false; false; true; true; false|];
      [|true; false; false; false; true; false; false|];
      [|false; false; false; false; false; true; true|];
      |]}

let i =
  of_list [(0,2); (1,3); (2,1); (3,4); (4,0)] 

let res = apply_iso i a 3  

let par = parse_vector [[0;1]; [2]; [0]] 4

let _ =
  printf "parse_vector:\n"; 
  print par;
  printf "get_vector:\n";
  List.iter (fun x ->
    printf "{ ";
    List.iter (printf "%d ") x;
    printf "} ") (get_vector par);
    printf "\n" *)   
