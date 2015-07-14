module type G = sig
    type t
    type edge_type
    val init : int -> t
    val states : t -> (Big.bg_key, int * Big.bg) Hashtbl.t
    val label : t -> (string, int) Hashtbl.t
    val edges : t -> (int, edge_type) Hashtbl.t
    val dest : edge_type -> int
    val string_of_arrow : edge_type -> string
  end

(* Limit *)		  
module type L = sig
    type t
    type occ
    val init : t
    val increment : t -> occ -> t
    (* is_greater a b = a > b *)
    val is_greater : t -> t -> bool
end

(* Execution statistics *)
module type S = sig
    type t
    type g
    val make : float -> g -> int -> t
  end
		  
(* Export functions *)
module MakeE (G : G) = struct
    
    let to_prism g =
      let (s, e) =
	(Hashtbl.length (G.states g), Hashtbl.length (G.edges g))
      and edges =
	Hashtbl.fold (fun v u acc ->
		      (v, u) :: acc) (G.edges g) [] in
      List.fast_sort (fun (v, u) (v', u') ->
		      Base.ints_compare (v, G.dest u) (v', G.dest u'))
		     edges
      |> List.map (fun (v, u) ->
		   (string_of_int v)
		   ^ " "
		   ^ (string_of_int (G.dest u))
		   ^ (match G.string_of_arrow u with
		      | "" -> ""
		      | s -> " " ^ s))
      |> List.append [(string_of_int s) ^ " " ^ (string_of_int e)]
      |> String.concat "\n"

    let to_dot g ~name =
      let rank = "{ rank=source; 0 };\n" in
      let states =
	Hashtbl.fold (fun _ (i, _) buff -> 
		      if i = 0 then
			Printf.sprintf 
			  "%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \
			   fontname=\"monospace\", fixedsize=true, width=.60, height=.30 \
			   style=\"bold\" ];\n" 
			  buff i i i i
		      else
			Printf.sprintf 
			  "%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \
			   fontname=\"monospace\", fixedsize=true, width=.60, height=.30 ];\n" 
			  buff i i i i)
		     (G.states g) ""
      and edges =
	Hashtbl.fold (fun v u buff -> 
		      Printf.sprintf
			"%s%d -> %d [ label=\"%s\", fontname=\"monospace\", fontsize=7.0,\
			 arrowhead=\"vee\", arrowsize=0.5 ];\n" 
			buff v (G.dest u) (G.string_of_arrow u))
		     (G.edges g) "" in
      Printf.sprintf "digraph \"%s\" {\nstylesheet = \"style_sbrs.css\"\n%s%s\n%s}"
		     name rank states edges
		     
    module StringSet = Set.Make (struct
				    type t = string
				    let compare = String.compare
				  end)

    let to_lab g =
      let h = G.label g in
      (* Set of label identifiers *)
      let ids = Hashtbl.fold (fun id _ acc ->
			      StringSet.add id acc)
			     h StringSet.empty in
      StringSet.fold (fun id acc ->
		      Hashtbl.find_all h id
		      |> List.map (fun s -> "x = " ^ (string_of_int s)) 
		      |> String.concat " | " 
		      |> (fun s -> "label \"" ^ id ^ "\" = " ^ s)
		      |> fun s -> s ::  acc)
		     ids []  
      |> List.rev
      |> String.concat ";\n"
		       
    let iter_states ~f g =
      Hashtbl.iter (fun _ (i, b) -> f i b) (G.states g)
		   
    let write_svg g ~name ~path =
      Export.write_svg (to_dot g ~name) ~name ~path
		       
    let write_prism g ~name ~path =
      Export.write_string (to_prism g) ~name ~path
			  
    let write_lab g ~name ~path =
      Export.write_string (to_lab g) ~name ~path
			  
    let write_dot g ~name ~path =
      Export.write_string (to_dot g ~name) ~name ~path
			  
  end

module Make (R : RrType.T)
	      (P : sig
		  type p_class =
		    | P_class of R.t list
		    | P_rclass of R.t list
		  val is_valid : p_class -> bool
		  val is_valid_list : p_class list -> bool
		  val rewrite : Big.bg -> p_class list -> Big.bg * int
		  val cardinal : p_class list -> int
		  val scan : Big.bg * int ->
			     part_f:(R.occ list ->
				     ((int * R.occ) list * R.edge list * int)) ->
			     const_pri:p_class list -> p_class list ->
			     ((int * R.occ) list * R.edge list * int) * int
		  val scan_sim : Big.bg ->
				 iter_f:(int -> Big.bg -> unit) ->
				 const_pri:p_class list -> p_class list ->
				 R.occ option * int
		end)
	      (L : L with type occ = R.occ)
	      (G : G with type edge_type = R.edge)
	      (S : S with type g = G.t) = struct

    type t = G.t
   		    
    include P

    type limit = L.t

    exception MAX of t * S.t
			   
    exception LIMIT of t * S.t

    exception DEADLOCK of t * S.t * limit

    (* Override some functions *)	       
    let to_string_react = R.to_string
			    
    let is_valid_react = R.is_valid
			   
    let fix = R.fix
		
    let step = R.step
		 
    let random_step = R.random_step
		    
    let is_valid_priority = is_valid
			  
    let is_valid_priority_list = is_valid_list
				      
    let is_new b v =
      let k_buket =
	Hashtbl.find_all v (Big.key b) in
      try
    	let (old, _) =
	  List.find (fun (_, b') ->
		     Big.equal_opt b b')
		    k_buket in
    	Some old            (* Is_new? FALSE *)
      with
      | Not_found -> None   (* Is_new? TRUE  *)
		       
    (* Partition a list of occurrences into new and old states *)
    let partition g i f_iter =
      List.fold_left (fun (new_acc, old_acc, i) o ->
		      let b = R.big_of_occ o in
    		      match is_new b (G.states g) with
		      | None ->
			 (let i' = i + 1 in (* Stop here when i > max *)
    			  f_iter i' b;
    			  ((i', o) :: new_acc, old_acc, i'))
    		      | Some x -> (new_acc, (R.edge_of_occ o x) :: old_acc, i))
    		     ([], [], i)

    (* Add labels for predicates *)		     
    let check (i, s) h =
      List.iter (fun (id, p) ->
		 if Big.occurs s p then
		   Hashtbl.add h id i 
		 else ())
      
    let rec _bfs g q i m t0 priorities predicates max iter_f =
      if not (Queue.is_empty q) then
	if i > max then
	  raise (MAX (g, S.make t0 g m))
	else 
	    (let (v, curr) = Queue.pop q in
	    let ((new_s, old_s, i'), m') = 
              P.scan (curr, i)
		     ~part_f:(partition g i iter_f)
		     ~const_pri:priorities priorities in
	    List.iter (fun (i, o) ->
		       let b = R.big_of_occ o in
		       (* Add new states to v *)
		       Hashtbl.add (G.states g) (Big.key b) (i, b);
		       (* Add labels for new states *)
		       check (i, b) (G.label g) predicates;
		       (* Add new states to q *)
		       Queue.push (i, R.big_of_occ o) q)
		      new_s;
	    (* Add edges from v to new states *)
	    List.iter (fun (u, o) -> 
		       Hashtbl.add (G.edges g) v (R.edge_of_occ o u))
		      new_s;
	    (* Add edges from v to old states *)
	    List.iter (fun e ->
		       Hashtbl.add (G.edges g) v e)
		      old_s;
	    (* recursive call *)
	    _bfs g q i' (m + m') t0 priorities predicates max iter_f) 
      else
	(g, S.make t0 g m)

    let bfs ~s0 ~priorities ~predicates ~max ~iter_f =
      let q = Queue.create () in
      (* Apply rewriting to s0 *)
      let (s0', m) = P.rewrite s0 priorities
      and g = G.init max in
      Queue.push (0, s0') q;
      (* Add initial state *)
      iter_f 0 s0';
      Hashtbl.add (G.states g) (Big.key s0') (0, s0');
      check (0, s0') (G.label g) predicates;
      _bfs g q 0 m (Unix.gettimeofday ()) priorities predicates max iter_f
	   					
    let rec _sim trace s i t_sim m t0 priorities predicates t_max iter_f =
      if L.is_greater t_sim t_max then
	raise (LIMIT (trace, S.make t0 trace m))
      else
	match P.scan_sim s
			 ~iter_f
			 ~const_pri:priorities
			 priorities with
	| (None, m') ->
	   raise (DEADLOCK (trace, S.make t0 trace (m + m'), t_sim))
	| (Some o, m') ->	
	   (let s' = R.big_of_occ o in
	    Hashtbl.add (G.states trace) (Big.key s') (i + 1, s');
	    check (i + 1, s') (G.label trace) predicates;
	    Hashtbl.add (G.edges trace) i (R.edge_of_occ o (i + 1));
	    _sim trace s' (i + 1) (L.increment t_sim o) (m + m')
		 t0 priorities predicates t_max iter_f) 
				    				    
    let sim ~s0 ~priorities ~predicates ~init_size ~stop ~iter_f =
      Random.self_init ();
      (* Apply rewriting to s0 *)
      let (s0', m) = P.rewrite s0 priorities
      and trace = G.init init_size in
      (* Add initial state *)
      iter_f 0 s0';
      Hashtbl.add (G.states trace) (Big.key s0') (0, s0');
      check (0, s0') (G.label trace) predicates;
      _sim trace s0' 0 L.init m (Unix.gettimeofday ())
	   priorities predicates stop iter_f
							    
    include MakeE (G)
	   
  end
