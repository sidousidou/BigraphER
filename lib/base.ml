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
    
module Nodes = struct
  
  type t = { 
    ctrl : (int, Ctrl.t) Hashtbl.t;
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
 
  let find_all s (Ctrl.Ctrl (n, _)) =
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
      add acc (Iso.find i iso) c) s (empty ())
  
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
  
  exception FOUND
      
  (* true when a contains a control that is not present in b *)
  let not_sub a b =
    try 
      Hashtbl.iter (fun c _ -> 
	if Hashtbl.mem b.sort c then ()
	else raise FOUND) a.sort;
      false
    with
    | FOUND -> true
 
  let norm s =
    String.concat ";"
      (List.fast_sort String.compare
	 (fold (fun _ (Ctrl.Ctrl (n, _)) acc ->
	   n :: acc
	  ) s [])
      )

  (* Simple string comparison *)
  let equal a b =
    (String.compare (norm a) (norm b)) = 0

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
    let h = Hashtbl.create (cardinal p) 
    and aux (Ctrl.Ctrl (s, _)) = s in
    iter (fun (i, _) ->
        Hashtbl.add h i (aux (Nodes.find n i))) p;
    let l = 
      fst (
        Hashtbl.fold (fun i _ (acc, marked) ->
            if List.mem i marked then (acc, marked)
            else (
	      let s =
	        String.concat "" (Hashtbl.find_all h i) in
              (s :: acc, i :: marked)
            )
          ) h ([], [])
      ) in
    List.fast_sort String.compare l

  let to_IntSet ps =
    fold (fun (i, _) acc -> 
      IntSet.add i acc) ps IntSet.empty

  let apply s iso =
    fold (fun (i, p) acc ->
        add (Iso.find i iso, p) acc
      ) s empty

  (* Compute the arities of the nodes within a port set. The output is a map 
     node -> arity *)
  let arities p =
    List.map (fun (i, js) ->
        (i, IntSet.cardinal js)
      ) (Rel.bindings (fold (fun (i, p) r ->
        Rel.add i (IntSet.singleton p) r
      ) p Rel.empty))
        
  let compat_list a b n_a n_b =
    let ar_a = arities a
    and ar_b = arities b
    and i_a = to_IntSet a 
    and i_b = to_IntSet b in
    IntSet.fold (fun i acc ->
        let ar_i = List.assoc i ar_a
        and c_i = Nodes.find n_a i in
        let pairs =
	  List.map (fun j -> 
	      Cnf.M_lit (i, j)
            ) (IntSet.elements (
              IntSet.filter (fun j ->
	          (ar_i = (List.assoc j ar_b)) && 
	          (Ctrl.(=) c_i (Nodes.find n_b j))
                ) i_b)) in 
        pairs :: acc
      ) i_a []

end   
