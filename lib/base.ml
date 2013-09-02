open Printf

module Ctrl = struct

  type t = Ctrl of string * int
  
  (* Debug - arity to be removed *)
  let to_string = function 
    | Ctrl (c, ar) -> sprintf "%s:%d" c ar

  let arity = function 
    | Ctrl (_, ar) -> ar

  let name = function 
    | Ctrl (s, _) -> 
      try 
	let il = String.index s '(' in 
	String.sub s 0 il
      with 
      | Not_found -> s
      
  let compare (Ctrl (c0, ar0)) (Ctrl (c1, ar1)) =
    match String.compare c0 c1 with
    | 0 -> ar0 - ar1
    | x -> x 
      
  let (=) c0 c1 =
    (compare c0 c1) = 0 
    
  (* Control(a0,a1,a2) --> [a0;a1;a2]*)
  let acts (Ctrl (s, _)) =
      try
	let il = String.index s '(' and ir = String.index s ')' in
	let s_acts = String.sub s (il + 1) ((ir - il) - 1)
	in Str.split (Str.regexp ",") s_acts
      with 
      | Not_found -> []
	
end

let ints_compare (i0, p0) (i1, p1) =
  match i0 - i1 with
  | 0 -> p0 - p1 
  | x -> x

module Iso = struct

  type t = (int, int) Hashtbl.t                   
 
  let empty = Hashtbl.create 20

  (* No duplicates *)
  let add iso i j = Hashtbl.replace iso i j  

  let find iso i = Hashtbl.find iso i
    
  let inverse iso = 
    let iso' = Hashtbl.create (Hashtbl.length iso) in
    Hashtbl.iter (fun i j ->
      Hashtbl.add iso' j i) iso;
    iso'
  
  let dom iso =
    Hashtbl.fold (fun i _ acc -> i :: acc) iso []

  let codom iso =
    Hashtbl.fold (fun _ j acc -> j :: acc) iso []
      
  let union a b =
    let u = Hashtbl.create ((Hashtbl.length a) + (Hashtbl.length b)) in 
    let f i j = add u i j in
    Hashtbl.iter f a;
    Hashtbl.iter f b;
    u

  let fold f iso acc = Hashtbl.fold f iso acc

  let iter f iso = Hashtbl.iter f iso
  
  let to_string iso =
    sprintf "{%s}" 
      (String.concat ", " 
	 (Hashtbl.fold (fun i j acc -> 
	   (sprintf "(%d, %d)" i j) :: acc) iso []))

  let of_list l =
    let iso = Hashtbl.create (List.length l) in
    List.iter (fun (i, j) ->
      Hashtbl.add iso i j) l;
    iso
      
  let mem iso i j = 
    try 
      j = Hashtbl.find iso i
    with
    | _ -> false

  let cardinal iso = Hashtbl.length iso

  let is_id iso =
    Hashtbl.fold (fun i j acc -> 
      (i = j) && acc) iso true

  (* input:  i : P -> T  autos : P -> P *)
  let gen_isos i autos =
    let apply iso a = 
      assert (cardinal iso = cardinal a);
      fold (fun i j acc ->
	add acc (find a i) j; 
	acc) iso empty in
    List.map (apply i) autos
      
end 

module IntSet = struct  
    
  include Set.Make (struct 
    type t = int
    let compare a b = a - b
  end)
    
  let to_string s =
    sprintf "{%s}"
      (String.concat "," (List.map string_of_int (elements s)))

  (* Transform an int list to an Int_set *)
  let of_list =
    List.fold_left (fun acc e -> 
      add e acc) empty
          
  (* given a non-nagative integer i return ordinal i = {0,1,....,i-1} i.e.   *)
  (* set with cardinality i                                                  *)
  let of_int i =
    let rec fold i acc =
      match i with
      | 0 -> acc
      | _ ->  fold (i - 1) (add i acc) in
    fold (i - 1) empty
      
  (* add offset i to every element in set s *)
  let off i s =
    fold (fun x acc -> 
      add (x + i) acc) s empty

  (* Normalises a set of integers e.g. [2;5;7;8] -- > [0;1;2;3] *)
  let norm s = 
    of_int (cardinal s)

  let apply s iso =
    assert (Iso.cardinal iso = cardinal s);
    fold (fun i acc ->
      add (Iso.find iso i) acc) s empty

  (* Generates an isomorphism to fix the numbering of a set of int. 
     [2;5;6;7] --> [(2,0),(5,1),(6,2),(7,3)]                           *)
  let fix s =
    let img = of_int (cardinal s)
    in Iso.of_list (List.combine (elements s) (elements img))
    
end
    
module Nodes = struct
  
  type t = { ctrl : (int, Ctrl.t) Hashtbl.t;
	     sort : (string, int) Hashtbl.t;
	     size : int;
	   }
  
  let empty = { ctrl = Hashtbl.create 50;
		sort = Hashtbl.create 50;
		size = 0;
	      } 
  
  let is_empty s = s.size = 0

  let add s i c =
    let aux (Ctrl.Ctrl (n, _)) = n in
    if Hashtbl.mem s.ctrl i then s
    else begin
      Hashtbl.add s.ctrl i c;
      Hashtbl.add s.sort (aux c) i;
      { s with size = s.size + 1; }
    end
  
  let fold f s acc = 
    Hashtbl.fold f s.ctrl acc

  let find s i =
    Hashtbl.find s.ctrl i
 
  let find_all s n =
    Hashtbl.find_all s.sort n
 
  let to_string s =
    sprintf "{%s}" 
      (String.concat "," 
	 (fold (fun i c acc ->
	   acc @ [sprintf "(%d, %s)" i (Ctrl.to_string c)]) 
	    s []))
      
  let to_dot s =
    String.concat "\n" 
      (fold (fun i c acc ->
	acc @ [sprintf "v%d [label=\"%s\", fontname=sans-serif];" i (Ctrl.to_string c)]) 
	 s [])

  let tens a b =
    Hashtbl.iter (fun i c ->
      Hashtbl.add a.ctrl (i + b.size) c) b.ctrl;
    Hashtbl.iter (fun c i ->
      Hashtbl.add a.sort c (i + b.size)) b.sort;
    { a with size = a.size + b.size; }
     
  (* is an ordered list of controls with duplicates *)
  let abs s = 
    List.fast_sort Ctrl.compare
      (fold (fun _ c acc ->
	c :: acc) s [])

  let apply s iso =
    assert (Iso.cardinal iso = s.size);
    fold (fun i c acc ->
      add acc (Iso.find iso i) c) s empty
  
end

module Ports = struct

  include Set.Make (struct
    (* node id, number of occurrences *) 
    type t = (int * int)   
    let compare = ints_compare
  end)

  let to_string ps = 
    sprintf "{%s}"
      (String.concat ", " (List.map (fun (a, b) ->
	sprintf "(%d, %d)" a b) (elements ps)))  

  let of_node (n, c) =
    let rec fold i acc =
      if i < 0 then acc
      else fold (i - 1) (add (n, i) acc) in
    fold ((Ctrl.arity c) - 1) empty

  (* Transform a set of nodes in a set of ports *)
  let of_nodes ns =
    Nodes.fold (fun n c acc -> 
      union (of_node (n, c)) acc) ns empty

  (* [(1,0);(1,1);(2,0)] -> [(1,2);(2,1)] 
     elements are (cardinality, node id) *)
  let multiset ps =
    fold (fun (p, _) acc ->
      try 
	let card = Iso.find acc p in
	Iso.add acc (card + 1) p;
	acc 
      with
      | _ -> begin
	Iso.add acc 1 p;
	acc
      end) ps Iso.empty
      
  (* Construct a list of the cardinalities of the ports belonging to
   the same node. [(1,0);(1,1);(2,0)] -> [1;2] *)
  let card_list p = 
    Iso.dom (multiset p)

  (* Construct a list of control strings [AA;BBBB;C]*)
  let types p n =
    let rec dup s n buff =
      if n = 0 then buff
      else dup s (n - 1) (s ^ buff) 
    and aux (Ctrl.Ctrl (s, _)) = s in
    fst (fold (fun (i, _) (acc, marked) ->
      let s = aux (Nodes.find n i) in
      if List.mem s marked then (acc, marked)
      else begin 
	let card = List.length (Nodes.find_all n s) in
	((dup s card "") :: acc, s :: marked)
      end
    ) p ([], []))

  let to_IntSet ps =
    fold (fun p acc -> 
      IntSet.add (fst p) acc) ps IntSet.empty

  let apply s iso =
    assert (Iso.cardinal iso = cardinal s);
    fold (fun (i, p) acc ->
      add (Iso.find iso i, p) acc) s empty
      
end   
  
(*       
(* sub multi-set *)
(* inputs are ordered lists with dupicates *)
(* is b a subset of a? *)
let sub_multi a b =
  let rec mem l e = 
    match l with
      | x :: xs -> if x = e then xs else mem xs e
      | [] -> failwith "Not found"
  in 
  try
    match List.fold_left mem a b with
      | _ -> true
  with
  | _ -> false		

let match_nodes t p =
  Nodes.fold (fun (j, c) acc ->
    Nodes.fold (fun (i, d) acc ->
      if ctrl_equals c d then
        acc
      else Iso.add (i,j) acc) p acc) t Iso.empty

(* Parameters handling *)
(* Removes duplicates and add rates. 
   Example: [(1, 1.5); (2, 0.5); (3, 1.); (2, 0.5); (1, 0.5)]
         [(3, 1.); (2, 1.); (1, 2.)]  *)
let count l f =
  let rec mem (x, rho) xs f acc =
    match xs with
      | (r, lambda) :: rs -> (if f x r then mem (x, rho +. lambda) rs f acc
	else mem (x, rho) rs f ((r, lambda) :: acc))
      | [] -> ((x, rho), acc)
  in let rec aux l f acc = 
       match l with
       | x :: xs -> (let (v, new_l) = mem x xs f []
		     in aux new_l f (v :: acc))
       | [] -> acc
     in aux l f [] 

(* cartesian product *)
let rec cart a b res = 
  match a with
    | [] -> res 
    | x  :: xs -> cart xs b (res @ (List.map (fun y -> x @ y) b))

let set_cart a b = 
  IntSet.fold (fun i acc ->
    Iso.union acc (IntSet.fold (fun j acc ->
      Iso.add (i, j) acc) b Iso.empty)) a Iso.empty

(* input is a list of sets. A set is represented as a list of lists *)
let cart_of_list l =
  let rec aux l res =
    match l with
    | [] -> res
    | x :: xs -> aux xs (cart res x []) in
  match l with
  | [] -> []
  | x :: [] -> x
  | x :: xs -> aux xs x 

(* Optimise *)  
let cart_of_list_iso l = 
  List.map of_list (cart_of_list (List.map (fun i ->
    List.map (fun pair -> [pair]) (Iso.elements i)) l))
*)
