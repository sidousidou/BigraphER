module type P = sig
    type t
    val f_val : t -> bool
    val f_r_val : t -> bool
  end
		  
module Make (R : RrType.T)
	    (P : P with type t = R.t list) : sig
    type p_class =
      | P_class of R.t list
      | P_rclass of R.t list
    val is_valid : p_class -> bool
    val is_valid_list : p_class list -> bool
    val is_reducible : p_class -> bool		
    val rewrite : Big.bg -> int -> p_class list -> Big.bg * int
    val scan : Big.bg * int -> matches:int ->
	       part_f:(R.occ list -> ((int * R.occ) list * R.edge list * int)) ->
	       const_pri:p_class list -> p_class list ->
	       ((int * R.occ) list * R.edge list * int) * int
  end
