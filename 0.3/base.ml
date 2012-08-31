open Printf

type ctrl = | Ctrl of string * int

let string_of_ctrl = function | Ctrl (c, _) -> c
let arity = function | Ctrl (_, ar) -> ar

let ctrl_equals (Ctrl (c0, ar0)) (Ctrl (c1, ar1)) =
  (c0 = c1) && (ar0 = ar1)

module Nodes =
  Set.Make
    (struct
       (*node id , control*) 
       type t = (int * ctrl)
       let compare (i0, Ctrl (c0, ar0)) (i1, Ctrl (c1, ar1)) =
         match compare i0 i1 with
          | 0 -> (match compare c0 c1 with
                   | 0 -> compare ar0 ar1
                   | x -> x)
          | x -> x  
     end)

module Ports =
  Set.Make
    (struct
       (*node id, number of occurrences*) 
       type t = (int * int)   
       let compare (i0, p0) (i1, p1) =
         match compare i0 i1 with
          | 0 -> compare p0 p1
          | x -> x
     end)
  
(* not really an Iso, just binary relation *)
module Iso = Set.Make(struct type t = (int * int)
                              let compare = compare
                                 end)                    
  
module Int_set = Set.Make(struct type t = int
                                  let compare = compare
                                     end)
  
let string_of_ports ps = 
  sprintf "{%s}"
  (String.concat ", " (List.map (fun (a, b) ->
    sprintf "(%d, %d)" a b) (Ports.elements ps)))  

(* raise Not_found *)
let ctrl_of_node i ns =
  let (_, c) = Nodes.choose (Nodes.filter (fun (x, _) -> x = i) ns) in c

(* raise Not_found*)  
let get_i n i =
  let (_, b) = Iso.choose (Iso.filter (fun (a, _) -> a = n) i) in b
  
(* returns the first binding *)
let get_inv_i n i =
  let (a, _) = Iso.choose (Iso.filter (fun (_, b) -> b = n) i) in a
  
(* apply iso i to every element in set s raise Not_found if an element is  *)
(* undefined in the iso                                                    *)
let apply s i =
  Int_set.fold (fun e acc -> Int_set.add (get_i e i) acc) s Int_set.empty
  
let of_list = List.fold_left (fun acc x -> Iso.add x acc) Iso.empty
  
let inverse i = Iso.fold (fun (a, b) acc -> Iso.add (b, a) acc) i Iso.empty
  
let uplus n0 n1 =
  let off = Nodes.cardinal n0 in
  let n1_off =
    Nodes.fold (fun (n, c) acc -> Nodes.add ((n + off), c) acc) n1 Nodes.
      empty
  in Nodes.union n0 n1_off
  
(* given a non-nagative integer i return ordinal i = {0,1,....,i-1} i.e.   *)
(* set with cardinality i                                                  *)
let rec of_int i =
  match i with
  | 0 -> Int_set.empty
  | _ -> Int_set.add (i - 1) (of_int (i - 1))
  
(* add offset i to every element in set s *)
let off i s =
  Int_set.fold (fun x acc -> Int_set.add (x + i) acc) s Int_set.empty
  
let dom i = Iso.fold (fun (a, _) acc -> Int_set.add a acc) i Int_set.empty
  
let codom i = Iso.fold (fun (_, b) acc -> Int_set.add b acc) i Int_set.empty

(* Transform an int list to an Int_set *)
let set_of_list =
	List.fold_left (fun acc e -> Int_set.add e acc) Int_set.empty

let set_of_ports ps =
  Ports.fold (fun p acc -> Int_set.add (fst p) acc) ps Int_set.empty

let ports_of_node (i, c) =
  Int_set.fold (fun p acc ->
    Ports.add (i, p) acc)(of_int (arity c)) Ports.empty

(* Transform a set of nodes in a set of ports *)
let ports_of_nodes ns =
  Nodes.fold (fun n acc -> Ports.union (ports_of_node n) acc) ns Ports.empty
 
(* Generates an isomorphism to fix the numbering of a set of nodes e.g.    *)
(* [2;5;6;7] --> [(2,0),(5,1),(6,2),(7,3)]                                 *)
let fix_num s =
  let img = of_int (Int_set.cardinal s)
  in of_list (List.combine (Int_set.elements s) (Int_set.elements img))
  
(* Normalises a set of integers e.g. [2;5;7;8] -- > [0;1;2;3] *)
let norm s = of_int (Int_set.cardinal s)
  
(* Apply iso to a set of nodes. i is defined for every element in ns. Some *)
(* elements in ns may not appear in the result if they are not in the      *)
(* domain of [i]                                                           *)
let apply_nodes ns i =
  Nodes.fold
    (fun (x, c) acc ->
       try let y = get_i x i in Nodes.add (y, c) acc with | Not_found -> acc)
    ns Nodes.empty
  
(* apply i to every element in ps and normalise them. every element in ps  *)
(* is in the doman of i.                                                   *)
let apply_ports ps i = (* fix node numbering *) (*	let out = *)
  Ports.fold (fun (x, p) acc -> Ports.add ((get_i x i), p) acc) ps Ports.
    empty
  
(* normalise ports *)
(*	in let rec aux temp res =                                        *)
(*		try                                                            *)
(*			let (x, _) = Ports.choose temp                               *)
(*			in let (p_x, rest) =                                         *)
(*				Ports.partition (fun (a, _) -> a = x) temp                 *)
(*			in let out =                                                 *)
(*				let n = of_int (Ports.cardinal p_x)                        *)
(*				in let rec f pl il acc =                                   *)
(*					match (pl, il) with                                      *)
(*					| ((a, _):: ps, i:: is) -> f ps is (Ports.add (a, i) acc)*)
(*					| ([], []) -> acc                                        *)
(*				in f (Ports.elements p_x) (Int_set.elements n) Ports.empty *)
(*			in aux rest (Ports.union res out)                            *)
(*		with                                                           *)
(*		| Not_found -> res                                             *)
(*	in aux out Ports.empty                                           *)
let string_of_nodes ns =
  "{" ^
    ((String.concat ", "
        (List.map
           (fun (n, c) ->
              "(" ^ ((string_of_int n) ^ (", " ^ ((string_of_ctrl c) ^ ")"))))
           (Nodes.elements ns)))
       ^ "}")
  
(* [ctrls] is a list of controls with no duplicates *)
(*let sort ctrls =
  let cs = List.map string_of_ctrl ctrls in
  let s = String.concat "|" cs in Ctrl s*)

(* is an ordered list of controls with duplicates *)
let abs_nodes ns = 
	List.fast_sort compare
		(List.map (fun (_, c) -> c) (Nodes.elements ns))	

(* sub multi-set *)
(* inputs are ordered lists with dupicates *)
(* is b a subset of a? *)
let sub_multi a b =
	let rec mem l e = 
		match l with
			| x::xs -> 
				(if x = e
					then xs
					else mem xs e
				)
			| [] -> failwith "Not found"
	in 
	try
		match	List.fold_left mem a b with
			| _ -> true
	with
		_ -> false		

(* transforms a binary relation into a function.
   Powerset construction with Hashtbl.find_all.*)
let hash_of_iso i =
  let h = Hashtbl.create (Int_set.cardinal (dom i)) in
  Iso.iter (fun (x, y) -> Hashtbl.add h x y) i;
  h
  
let name_of_ctrl c =
  let s = string_of_ctrl c
  in
    try let il = String.index s '(' in String.sub s 0 il
    with | Not_found -> s
  
(* Control(a0,a1,a2) --> [a0;a1;a2]*)
let acts_of_ctrl c =
  let s = string_of_ctrl c
  in
    try
      let il = String.index s '(' and ir = String.index s ')' in
      let s_acts = String.sub s (il + 1) ((ir - il) - 1)
      in Str.split (Str.regexp ",") s_acts
    with | Not_found -> []
  
let string_of_Int_set s =
  Printf.sprintf "{%s}"
    (String.concat "," (List.map string_of_int (Int_set.elements s)))
  
(* Parameters handling *)
(* Compute a list of values starting from s to e with interval h
   inbetween each value. e is always returned *)
let int_interval s h e =
  let rec aux s h e res =
    if s < e
    then aux (s + h) h e (res @ [s])
    else (res @ [e])
  in aux s h e []
  
let rec float_interval s h e =
  let rec aux s h e res =
    if s < e
    then aux (s +. h) h e (res @ [s])
    else (res @ [e])
  in aux s h e []

let rec par_comb pars = 
    match pars with
    | [x] -> List.map (fun v -> [v]) x
    | x::xs -> 
      begin
	let aux1 v ls =
	  List.map (fun l -> v::l) ls
	in let rec aux2 l ls = 
	     match l with
	     | [] -> []
	     | x::xs -> (aux1 x ls) @ (aux2 xs ls)
	   in aux2 x (par_comb xs) 
      end
    | [] -> []

(* Removes duplicates and add rates. 
   Example: [(1, 1.5); (2, 0.5); (3, 1.); (2, 0.5); (1, 0.5)]
         [(3, 1.); (2, 1.); (1, 2.)]  *)
let count l f =
	let rec mem (x, rho) xs f acc =
		match xs with
			| (r, lambda)::rs -> (if f x r
				then mem (x, rho +. lambda) rs f acc
				else mem (x, rho) rs f ((r, lambda)::acc)
				)
			| [] -> ((x, rho), acc)
	in let rec aux l f acc = 
		match l with
			| x::xs -> (let (v, new_l) = mem x xs f []
				in aux new_l f (v::acc)
			 )
			| [] -> acc
in aux l f [] 

let get_dot ns =
  Nodes.fold (fun (i, c) buff ->
   sprintf "%sv%d [label=\"%s\", fontname=Arial];\n" (*, shape=circle*)
   buff i (string_of_ctrl c)) ns ""
     
