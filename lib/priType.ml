module type P = sig
    type t
    val f_val : t -> bool
    val f_r_val : t -> bool
  end
		  
module Make (R :RrType.T)
	    (P : P with type t = R.t list) =
  struct

    type p_class =
      | P_class of R.t list 
      | P_rclass of R.t list
			
    let is_valid = function
      | P_class []
      | P_rclass [] -> false	
      | P_class rr -> (List.for_all R.is_valid rr)
                      && P.f_val rr
      | P_rclass rr -> (List.for_all R.is_valid rr)
                       && P.f_r_val rr
			    		
    let is_valid_list =
      List.exists (function
		    | P_class _ -> true
		    | P_rclass _ -> false)
		  
    let is_enabled b =
      List.exists (fun r -> Big.occurs b (R.lhs r))

    let is_reducible = function
      | P_class _ -> false
      | P_rclass _ -> true
		  
    let rec rewrite b m = function
      | [] -> (b, m)
      | (P_class rr) :: classes ->
	 (if is_enabled b rr then (b, m)
          else rewrite b m classes)
      | (P_rclass rr) :: classes ->
	 (let (b', i) = R.fix b rr in
          rewrite b' (m + i) classes)

    (* Iterate over priority classes *)
    let rec scan (b, i) ~matches ~part_f ~const_pri = function
      | [] -> (([], [], i), matches)
      | (P_class rr) :: cs ->
         (let (ss, l) = R.step b rr in
          if l = 0 then scan (b, i) ~matches ~part_f ~const_pri cs 
          else 
	    (* apply rewriting - instantaneous *)
	    let (ss', l') = 
	      List.fold_left (fun (ss,  l) o -> 
			      let (s', l') =
				rewrite (R.big_of_occ o) l const_pri in
			      ((R.update_occ o s') :: ss, l'))
			     ([], l) ss in
	    ((part_f ss' : (int * R.occ) list * R.edge list * int), matches + l'))
      | P_rclass _ :: cs -> (* skip *)
         scan (b, i) ~matches ~part_f ~const_pri cs

  end
