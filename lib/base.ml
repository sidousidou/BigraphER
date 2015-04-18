open Printf

let safe = function
  | Some v -> v
  | None -> assert false

module Ctrl = struct

    type t = Ctrl of string * int
				
    let to_string = function 
      | Ctrl (c, ar) -> c ^ ":" ^ (string_of_int ar)
				    
    let arity = function 
      | Ctrl (_, ar) -> ar
			  
    let compare (Ctrl (c0, ar0)) (Ctrl (c1, ar1)) =
      match String.compare c0 c1 with
      | 0 -> ar0 - ar1
      | x -> x 
	       
    let (=) c0 c1 =
      (compare c0 c1) = 0 
    			  
  end

let ints_compare (i0, p0) (i1, p1) =
  match i0 - i1 with
  | 0 -> p0 - p1 
  | x -> x
    
module Nodes = struct

    type t =
      { ctrl : (int, Ctrl.t) Hashtbl.t;
	sort : (string, int) Hashtbl.t;
	size : int; }
  
    let empty () =
      { ctrl = Hashtbl.create 20;
	sort = Hashtbl.create 20;
	size = 0; } 
  
  let is_empty s = s.size = 0

  let add s i = function
    | Ctrl.Ctrl (n, _) as c ->
       assert (i >= 0);
       if Hashtbl.mem s.ctrl i then s
       else
	 (Hashtbl.add s.ctrl i c;
	  Hashtbl.add s.sort n i;
	  { s with size = s.size + 1; })
  
  let fold f s = 
    Hashtbl.fold f s.ctrl

  let get_ctrl s i =
    assert (i >= 0);
    Hashtbl.find s.ctrl i
 
  let find_all s (Ctrl.Ctrl (n, _)) =
    Hashtbl.find_all s.sort n
 
  let to_string s =
    "{"
    ^ (fold (fun i c acc ->
	     acc @ ["("
		    ^ (string_of_int i)
		    ^ ", "
		    ^ (Ctrl.to_string c)
		    ^ ")"]) 
	    s []
       |> String.concat ",")
    ^ "}" 
      
  let to_dot s =
    fold (fun i (Ctrl.Ctrl (n, _)) acc ->
	  acc @ [sprintf "v%d [ label=\"%s\", shape=ellipse, id=\"v%d_%s\" \
                          fontname=\"sans-serif\", fontsize=9.0,\
                          fixedsize=true, width=%f, height=.30 ];" 
			 i n i n (0.1 *. (float (String.length n)) +. 0.2)])
	 s []
    |>  String.concat "\n" 
	
  let tens a b =
    { ctrl = Hashtbl.copy a.ctrl;
      sort = Hashtbl.copy a.sort;
      size = a.size }
    |> Hashtbl.fold (fun i c res ->
		  add res (i + a.size) c) b.ctrl
      
  let apply_exn s iso =
    fold (fun i c acc ->
	  add acc (Iso.find_exn i iso) c)
	 s (empty ())
  
  (* Only nodes in the domain of the isomorphism are transformed. Other nodes 
     are discarded. *)
  let filter_apply_iso s iso =    
    Iso.fold (fun i j acc ->
	      try 
		Hashtbl.find s.ctrl i
		|> add acc j
	      with
	      | Not_found -> acc)
	     iso (empty ())

  let parse s h =
    Str.split (Str.regexp_string " ") s
    |> List.fold_left (fun (acc, i) t ->
		       let ar = 
			 try
			   Hashtbl.find h i
			 with
			 | Not_found -> 0 in
		       let c = Ctrl.Ctrl (t, ar) in
		       (add acc i c, i + 1))
		      (empty (), 0)
    |> fst
	
  exception FOUND
      
  (* true when a contains a control that is not present in b *)
  let not_sub a b =
    try 
      Hashtbl.iter (fun c _ -> 
		    if Hashtbl.mem b.sort c then ()
		    else raise_notrace FOUND) a.sort;
      false
    with
    | FOUND -> true
 
  let norm s =
    fold (fun _ (Ctrl.Ctrl (n, _)) acc ->
	  n :: acc)
	 s []
    |> List.fast_sort String.compare
    |> String.concat ";"

  (* Simple string comparison *)
  let equal a b =
    (String.compare (norm a) (norm b)) = 0

end

module PortSet = struct

    (* node id, number of occurrences *) 
    type port = int * int
			
    include Set.Make (struct
			 type t = port
			 let compare = ints_compare
		       end)

    let to_string ps =
      "{"
      ^ (elements ps
	 |> List.map (fun (a, b) ->
		      "("
		      ^ (string_of_int a)
		      ^ ", "
		      ^ (string_of_int b)
		      ^ ")")
	 |> String.concat ", ")
      ^ "}"
	       
    (* Transform a set of nodes in a set of ports *)
    let of_nodes ns =
      let of_node (n, c) =
	assert (n >= 0);
	let rec fold i acc =
	  if i < 0 then acc
	  else fold (i - 1) (add (n, i) acc) in
	fold ((Ctrl.arity c) - 1) empty in
      Nodes.fold (fun n c acc -> 
		  union (of_node (n, c)) acc) ns empty

    (* Construct a list of control strings [AA;BBBB;C]*)
    let types p n =
      let h = Hashtbl.create (cardinal p) 
      and aux (Ctrl.Ctrl (s, _)) = s in
      iter (fun (i, _) ->
            Hashtbl.add h i (aux (Nodes.get_ctrl n i))) p;
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

    let apply_exn s iso =
      fold (fun (i, p) acc ->
	    add (Iso.find_exn i iso, p) acc) s empty

    (* Compute the arities of the nodes within a port set. The output is a map 
       node -> arity *)
    let arities p =
      let rel =
	fold (fun (i, p) r ->
	      Rel.add i (IntSet.singleton p) r) p Rel.empty in 
      Rel.fold (fun i ports acc ->
		Fun.add i (IntSet.cardinal ports) acc) rel Fun.empty
		  
    let compat_list a b n_a n_b =
      let ar_a = arities a
      and ar_b = arities b
      and i_a = to_IntSet a 
      and i_b = to_IntSet b in
      IntSet.fold (fun i acc ->
		   let ar_i = safe (Fun.find i ar_a)
		   and c_i = Nodes.get_ctrl n_a i in
		   let pairs =
		     IntSet.filter (fun j ->
				    (ar_i = safe (Fun.find j ar_b))
				    && (Ctrl.(=) c_i (Nodes.get_ctrl n_b j)))
				   i_b
		     |> IntSet.elements
		     |> List.map (fun j -> Cnf.M_lit (i, j)) in 
		   pairs :: acc)
		  i_a []

  end   
