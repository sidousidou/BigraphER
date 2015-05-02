module type R =
  sig
    type t
    type label
    type occ
    type edge
    val lhs : t -> Big.bg
    val rhs : t -> Big.bg
    val l : t -> label
    val string_of_label : label -> string
    val map : t -> int Fun.t option
    val val_chk : t -> bool
    val to_occ : Big.bg -> t -> occ
    val big_of_occ : occ -> Big.bg
    val merge_occ : occ -> occ -> occ
    val update_occ : occ -> Big.bg -> occ
    (* Replace the bigraph in an occurrence with an index *)
    val edge_of_occ : occ -> int -> edge 
  end

module type T =
  sig
    type t
    type label
    type occ
    type edge
    val lhs : t -> Big.bg
    val rhs : t -> Big.bg
    val l : t -> label
    val string_of_label : label -> string
    val map : t -> int Fun.t option
    val to_occ : Big.bg -> t -> occ
    val big_of_occ : occ -> Big.bg
    val merge_occ : occ -> occ -> occ
    val update_occ : occ -> Big.bg -> occ
    val edge_of_occ : occ -> int -> edge
    val to_string : t -> string
    val is_valid : t -> bool
    val is_enabled : Big.bg -> t -> bool
    val fix : Big.bg -> t list -> Big.bg * int
    val step : Big.bg -> t list -> occ list * int
  end
		  
module Make (R : R) = struct
    
    include R
		
    let to_string r =
      (Big.to_string (lhs r))
      ^ "\n--"
      ^ (string_of_label (l r))
      ^ "-->\n"
      ^	(Big.to_string (rhs r))
      ^ (match map r with
	 | None -> ""
	 | Some eta -> "\n@ " ^ (Fun.to_string eta))

    let is_valid r =
      let lhs = lhs r
      and rhs = rhs r in
      (Big.inter_equal (Big.outer lhs) (Big.outer rhs))
      && (lhs.Big.p.Place.n > 0)
      && (Big.is_solid lhs)
      && (match map r with
      	  | None -> Big.inter_equal (Big.inner lhs) (Big.inner rhs)
      	  | Some eta ->
      	     (let s_lhs = lhs.Big.p.Place.s
      	      and s_rhs = rhs.Big.p.Place.s in
      	      (Fun.is_total s_rhs eta)
      	      && (Fun.check_codom 0 (s_lhs - 1) eta)))
      && (val_chk r) 

    let is_enabled b r =
      Big.occurs b (lhs r)

    (* Reduce a reducible class to the fixed point. Return the input state if no
     rewriting is performed. *)
    let fix b rules =
      let t_trans = Sparse.trans b.Big.p.Place.nn in
      let rec _step s = function
	| [] -> None
	| r :: rs ->
	   (match Big.occurrence s (lhs r) t_trans with
	    | Some o ->
	       Some (Big.rewrite o s (lhs r) (rhs r) (map r))
	    | None -> _step s rs) in
      let rec _fix s rules i =
	match _step s rules with
	| Some b ->  _fix b rules (i + 1)
	| None -> (s, i) in
      _fix b rules 0

    (* Input list is assumed without duplicates. Examlpe: extract 4
       [0;2;3;4;5;6;7] -> (4, [0;2;3;5;6;7]) *)	   
    let rec extract (pred :'a -> bool) acc = function
      | [] -> (None, acc)
      | x :: l -> if pred x then (Some x, l @ acc)
		  else extract pred (x :: acc) l
	   
    let step b rules =
      let filter_iso (l : occ list) =
	let aux1 acc o =
	  let (iso, non_iso) =
	    extract (fun o' ->
		     Big.equal (big_of_occ o) (big_of_occ o'))
		    [] acc in
	  match iso with
	  | None -> o :: acc
	  | Some iso_o -> (merge_occ o iso_o) :: non_iso in
	(List.fold_left aux1 [] l, List.length l) in
      let aux2 acc r =
	(Big.occurrences b (lhs r) 
	 |> List.map (fun o ->
		      to_occ
			(Big.rewrite o b (lhs r) (rhs r) (map r)) r))
	@ acc in
      List.fold_left aux2 [] rules
      |> filter_iso
				
  end
