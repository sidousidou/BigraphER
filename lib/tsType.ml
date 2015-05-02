module type G = sig
    type t
    type edge_type
    val init : int -> t
    val states : t -> (Big.bg_key, (int * Big.bg)) Hashtbl.t
    val label : t -> (int, int) Hashtbl.t
    val edges : t -> (int, edge_type) Hashtbl.t
    val dest : edge_type -> int
    val string_of_arrow : edge_type -> string
  end

(* Limit *)		  
module type L = sig
    type t
    (* is_greater a b = a > b *)
    val is_greater : t -> t -> bool
end

(* Stats *)		  
module type S = sig
    type t
    val init : t0:float -> t
    val update : time:float -> states:int -> reacts:int -> occs:int ->
		 old_stats:t -> t		  
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
		      if i = 0 then Printf.sprintf 
				      "%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \
				       fontname=\"monospace\", fixedsize=true, width=.60, height=.30 \
				       style=\"bold\" ];\n" 
				      buff i i i i
		      else Printf.sprintf 
			     "%s%d [ label=\"%d\", URL=\"./%d.svg\", fontsize=9.0, id=\"s%d\", \
			      fontname=\"monospace\", fixedsize=true, width=.60, height=.30 ];\n" 
			     buff i i i i)
		     (G.states g) ""
      and edges =
	Hashtbl.fold (fun v u buff -> 
		      Printf.sprintf "%s%d -> %d [ label=\"%s\", fontname=\"monospace\", fontsize=7.0,\
				      arrowhead=\"vee\", arrowsize=0.5 ];\n" 
				     buff v (G.dest u) (G.string_of_arrow u))
		     (G.edges g) "" in
      Printf.sprintf "digraph %s {\nstylesheet = \"style_sbrs.css\"\n%s%s\n%s}"
		     name rank states edges

		     
    let to_lab g =
      let inv =
	Hashtbl.create (Hashtbl.length (G.label g)) in
      Hashtbl.fold (fun s p acc -> 
		    Hashtbl.add inv p s;
		    p :: acc)
		   (G.label g) []
      |>  List.map (fun p ->
		    Hashtbl.find_all inv p
		    |> List.map (fun s -> "x = " ^ (string_of_int s)) 
		    |> String.concat " | " 
		    |> fun s ->
		       "label \"p_" ^ (string_of_int p) ^ "\" = " ^ s ^ ";")
      |> String.concat "\n"

		       
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
  		  
module MakeTS (RT : RrType.R)
	      (PT : PriType.P with type t = RT.t list)
	      (S : S)
	      (G : G with type edge_type = RT.edge) = struct

    module R = RrType.Make (RT)
			   
    include PriType.Make (R) (PT)

    type t = G.t
    type stats = S.t
	       
    exception MAX of t * stats
	       
    let is_new b v =
      let k_buket =
	Hashtbl.find_all v (Big.key b) in
      try
    	let (old, _) =
	  List.find (fun (_, b') ->
		     Big.equal b b')
		    k_buket in
    	Some old            (* Is_new? FALSE *)
      with
      | Not_found -> None   (* Is_new? TRUE  *)
		       
    (* Partition a list of bigraphs into new and old states *)
    let partition g i f_iter =
      List.fold_left (fun (new_acc, old_acc, i) o ->
		      let b = RT.big_of_occ o in
    		      match is_new b (G.states g) with
		      | None ->
			 (let i' = i + 1 in
    			  f_iter i' b;
    			  ((i', o) :: new_acc, old_acc, i'))
    		      | Some x -> (new_acc, (RT.edge_of_occ o x) :: old_acc, i))
    		     ([], [], i)

    (* Iterate over priority classes *)
    let rec scan curr m g i iter_f ~const_priorities = function
      | [] -> (([], [], i), m)
      | (P_class rr) :: cs ->
         (let (ss, l) = R.step curr rr in
          if l = 0 then scan curr m g i iter_f ~const_priorities cs 
          else 
	    (* apply rewriting - instantaneous *)
	    let (ss', l') = 
	      List.fold_left (fun (ss,  l) o -> 
			      let (s', l') =
				rewrite (RT.big_of_occ o) l const_priorities in
			      ((RT.update_occ o s') :: ss, l'))
			     ([], l) ss in
	    (partition g i iter_f ss', m + l'))
      | P_rclass _ :: cs -> (* skip *)
         scan curr m g i iter_f ~const_priorities cs

    let rec _bfs g q i m stats priorities max iter_f =
      if not (Queue.is_empty q) then
	if i > max then
	  raise (MAX (g, S.update ~time:(Unix.gettimeofday ())
				  ~states:(Hashtbl.length (G.states g))
				  ~reacts:(Hashtbl.length (G.edges g))
				  ~occs:m
				  ~old_stats:stats))
	else 
	    (let (v, curr) = Queue.pop q in
	    let ((new_s, old_s, i'), m') = 
              scan curr m g i iter_f ~const_priorities:priorities priorities in
	    (* Add new states to v *)
	    List.iter (fun (i, o) ->
		       let b = RT.big_of_occ o in 
		       Hashtbl.add (G.states g) (Big.key b) (i, b))
		      new_s;
	    (* Add new states to q *)
	    List.iter (fun (i, o) ->
		       Queue.push (i, RT.big_of_occ o) q)
		      new_s;
	    (* Add labels for new states *)
	    (* TO DO *)
	    (* Add edges from v to new states *)
	    List.iter (fun (u, o) -> 
		       Hashtbl.add (G.edges g) v (RT.edge_of_occ o u))
		      new_s;
	    (* Add edges from v to old states *)
	    List.iter (fun e ->
		       Hashtbl.add (G.edges g) v e)
		      old_s;
	    (* recursive call *)
	    _bfs g q i' m' stats priorities max iter_f) 
      else
	(g, S.update ~time:(Unix.gettimeofday ())
		     ~states:(Hashtbl.length (G.states g))
		     ~reacts:(Hashtbl.length (G.edges g))
		     ~occs:m
		     ~old_stats:stats)

    let bfs ~s0 ~priorities ~max ~iter_f =
      let q = Queue.create () in
      (* apply rewriting to s0 *)
      let (s0', m) = rewrite s0 0 priorities
      and g = G.init max
      and stats = S.init ~t0:(Unix.gettimeofday ()) in
      Queue.push (0, s0') q;
      (* add initial state *)
      Hashtbl.add (G.states g) (Big.key s0') (0, s0');
      iter_f 0 s0';
      _bfs g q 0 m stats priorities max iter_f

    include MakeE (G)
	   
  end
