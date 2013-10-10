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
 
  let empty () = Hashtbl.create 20

  (* No duplicates *)
  let add iso i j = 
    assert (i >= 0);
    assert (j >= 0);
    Hashtbl.replace iso i j  

  let find iso i = 
    assert (i >= 0);
    Hashtbl.find iso i
    
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
  
  exception COMPARE of int  

  let subseteq a b =     
    try
      iter (fun i j ->
	match j - (find b i) with
	| 0 -> ()
	| x -> raise (COMPARE x)) a;
      0
    with
    | Not_found -> 1
    | COMPARE x -> x

  let compare a b = 
    let x = subseteq a b
    and y = subseteq b a in
    match x with
    | 0 -> y
    | _ -> x
      
  let equal a b = 
    compare a b = 0  

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

  let to_list  iso =
    fold (fun i j acc -> (i, j) :: acc) iso []

  let mem iso i j = 
    assert (i >= 0);
    assert (j >= 0);
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
	acc) iso (empty ()) in
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
    assert (i >= 0);
    let rec fold i acc =
      match i with
      | 0 -> acc
      | _ ->  fold (i - 1) (add (i - 1) acc) in
    fold i empty
      
  (* add offset i to every element in set s *)
  let off i s =
    fold (fun x acc -> 
      add (x + i) acc) s empty

  (* Normalises a set of integers e.g. [2;5;7;8] -- > [0;1;2;3] *)
  let norm s = 
    of_int (cardinal s)

  let apply s iso =
    assert (Iso.cardinal iso >= cardinal s);
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
  
  let empty () = { ctrl = Hashtbl.create 20;
		   sort = Hashtbl.create 20;
		   size = 0;
		 } 
  
  let is_empty s = s.size = 0

  let add s i c =
    assert (i >= 0);
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
    assert (i >= 0);
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
      (fold (fun i (Ctrl.Ctrl (n, _)) acc ->
	acc @ [sprintf "v%d [ label=\"%s\", shape=ellipse,\
                              fontname=\"sans-serif\", fontsize=9.0,\
                              fixedsize=true, width=%f, height=.30 ];" 
		  i n (0.1 *. (float (String.length n)) +. 0.2)]) s [])

  let tens a b =
    let a' = { ctrl = Hashtbl.copy a.ctrl;
	       sort = Hashtbl.copy a.sort;
	       size = a.size } in
    Hashtbl.fold (fun i c res ->
      add res (i + a.size) c) b.ctrl a'
      
  (* is an ordered list of controls with duplicates *)
  let abs s = 
    List.fast_sort Ctrl.compare
      (fold (fun _ c acc ->
	c :: acc) s [])

  let apply_iso s iso =
    assert (Iso.cardinal iso >= s.size);
    fold (fun i c acc ->
      add acc (Iso.find iso i) c) s (empty ())
  
  (* Only nodes in the domain of the isomorphism are transformed. Other nodes are discarded. *)    
  let filter_apply_iso s iso =    
    Iso.fold (fun i j acc ->
      try 
	let c = Hashtbl.find s.ctrl i in
	add acc j c
      with
      | Not_found -> acc) iso (empty ())

  let parse s h =
    let tokens = Str.split (Str.regexp_string " ") s in
    fst (List.fold_left (fun (acc, i) t ->
      let ar = 
	try
	  Hashtbl.find h i
	with
	| Not_found -> 0 in
      let c = Ctrl.Ctrl (t, ar) in
      (add acc i c, i + 1)) (empty (), 0) tokens)
      
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
    assert (n >= 0);
    let rec fold i acc =
      if i < 0 then acc
      else fold (i - 1) (add (n, i) acc) in
    fold ((Ctrl.arity c) - 1) empty

  (* Transform a set of nodes in a set of ports *)
  let of_nodes ns =
    Nodes.fold (fun n c acc -> 
      union (of_node (n, c)) acc) ns empty

  (* Construct a list of control strings [AA;BBBB;C]*)
  let types p n =
    let h = Hashtbl.create n.Nodes.size 
    and aux (Ctrl.Ctrl (s, _)) = s in
    iter (fun (i, _) ->
      Hashtbl.add h i (aux (Nodes.find n i))) p;
    let l = 
      fst (Hashtbl.fold (fun i _ (acc, marked) ->
      if List.mem i marked then (acc, marked)
      else begin
	let s =
	  String.concat "" (Hashtbl.find_all h i) in
	(s :: acc, i :: marked)
      end) h ([], [])) in
    List.fast_sort String.compare l

  let to_IntSet ps =
    fold (fun p acc -> 
      IntSet.add (fst p) acc) ps IntSet.empty

  let apply s iso =
    assert (Iso.cardinal iso = cardinal s);
    fold (fun (i, p) acc ->
      add (Iso.find iso i, p) acc) s empty

  (* normalise a port set: 
     INPUT:  {(1, 1), (1, 2), (2, 3), (3, 5)}
     OUTPUT: {(1, 0), (1, 1), (2, 0), (3, 0)} *)
  let norm ps =
    let h = Hashtbl.create (cardinal ps) in
    fold (fun (i, p) res ->
      let p' = List.length (Hashtbl.find_all h i) in
      Hashtbl.add h i p;
    add (i, p') res) ps empty

  let sub_multiset a b =
    subset (norm a) (norm b)

  (* Compute the arities of the nodes within a port set. The output is an iso 
     node -> arity *)
  let arities p =
    let h = Hashtbl.create (cardinal p) in
    iter (fun (i, p) -> Hashtbl.add h i p) p;
    let iso = Iso.empty () in 
    ignore (Hashtbl.fold (fun i _ marked ->
      if IntSet.mem i marked then marked
      else begin
	Iso.add iso i (List.length (Hashtbl.find_all h i));
	IntSet.add i marked
      end) h IntSet.empty);
    iso

  let compat_list a b n_a n_b =
    let ar_a = arities a
    and ar_b = arities b
    and i_a = to_IntSet a 
    and i_b = to_IntSet b in
    IntSet.fold (fun i acc ->
      let ar_i = Iso.find ar_a i
      and c_i = Nodes.find n_a i in
      let pairs =
	List.map (fun j -> 
	  Cnf.M_lit (i, j)) 
	  (IntSet.elements (IntSet.filter (fun j ->
	    (ar_i = (Iso.find ar_b j)) && 
	      (Ctrl.(=) c_i (Nodes.find n_b j))) i_b)) in 
      pairs :: acc) i_a []

end   
