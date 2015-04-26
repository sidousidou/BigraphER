module type P = sig
   
    val f_val : 'a list -> bool
    val f_r_val : 'a list -> bool
	   
  end

module type T = sig

    type p_class

    type r	   

    (* val make_class : r list -> p_class *)
				 
    (* val make_red_class : r list -> p_class *)
      
    val is_valid : p_class -> bool

    val is_valis_list : p_class list -> bool

    val is_enabled : p_class -> bool					  

    val rewrite : Big.bg -> int -> p_class list -> Big.bg * int
				  
  end
		  
module Make (R : RrType.T) (P : P) = struct
    
    type p_class =
      | P_class of R.t list 
      | P_rclass of R.t list

    (* let make_class rr = P_class rr *)

    (* let make_red_class rr = P_rclass rr *)
			
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

    let rec rewrite b m = function
      | [] -> (b, m)
      | (P_class rr) :: classes ->
	 (if is_enabled b rr then (b, m)
          else rewrite b m classes)
      | (P_rclass rr) :: classes ->
	 (let (b', i) = R.fix b rr in
          rewrite b' (m + i) classes)

  end
