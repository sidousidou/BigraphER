module type G = sig

    type t
    type edge_type
    type f
    type stats
    type limit
    type p_class
    val init : int -> t
    val states : t -> (Big.bg_key, (int * Big.bg)) Hashtbl.t
    val label : t -> (int, int) Hashtbl.t
    val edges : t -> (int, edge_type) Hashtbl.t
    val dest : edge_type -> int
    val arrow : edge_type -> f
    val string_of_arrow : edge_type -> string
  end

module type T = sig
    type t
    type stats
    type limit
    type p_class
	   
    exception LIMIT of t * stats

    val bfs : Big.bg -> p_class list -> int -> (int -> Big.bg -> unit) ->
	      t * stats
    val sim : Big.bg -> p_class list -> limit -> (int -> Big.bg -> unit) ->
	      t * stats
    val to_prism : t -> string
    val to_lab : t -> string
    val to_dot : t -> string
    val iter_states : (int -> Big.bg -> unit) -> t -> unit
  end

		  
module Make (G : G) = struct

    type t = G.t

    (* let is_new b v = *)
    (*   let k = Big.key b in *)
    (*   let k_buket =  *)
    (* 	Hashtbl.find_all v k in *)
    (*   try  *)
    (* 	let (old, _) = List.find (fun (_, b') -> *)
    (* 				  Big.equal b b') k_buket in *)
    (* 	raise (OLD old) *)
    (*   with *)
    (*   | Not_found -> true *)
		       
    (* (\* Partition a list of bigraphs into new and old states *\) *)
    (* let _partition_aux ts i f_iter = *)
    (*   List.fold_left (fun (new_acc, old_acc, i) b -> *)
    (* 		      try  *)
    (* 			ignore (is_new b ts.v); *)
    (* 			let i' = i + 1 in *)
    (* 			f_iter i' b; *)
    (* 			((i', b) :: new_acc, old_acc, i') *)
    (* 		      with *)
    (* 		      | OLD x -> (new_acc, x :: old_acc, i) *)
    (* 		     ) ([], [], i)	        *)

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

    let to_dot g =
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
      Printf.sprintf "digraph ctmc {\nstylesheet = \"style_sbrs.css\"\n%s%s\n%s}" rank states edges

	       
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

	       
    let iter_states f g =
      Hashtbl.iter (fun _ (i, b) -> f i b) (G.states g)

  end
