module type OccurType = sig
    type t (* (bg * float) or bg or (bg * int) *)
  end
			  
module type T = sig
    type t
    type occ
    val to_string : t -> string
    val is_valid : t -> bool
    val is_enabled : Big.bg -> t -> bool
    val fix : Big.bg -> t list -> Big.bg * int
    val step : Big.bg -> t list -> occ list * int
    val random_step : Big.bg -> t list -> occ * int
  end


module Make(O: OccurType) = struct
		  
    type t

    let lhs = function
      | (r, _, _) -> r

    let rhs = function
      | (_, r', _) -> r'

    let map = function
      | (_, _, eta) -> eta

    let to_string (r, r', eta) =
      sprintf "%s\n---->\n%s\neta: %s"
	      (Big.to_string r)
	      (Big.to_string r')
	      (Fun.to_string eta)

    let is_valid_react (r, r', eta) =
      (Big.inter_equal (Big.inner r) (Big.inner r')) &&
	(Big.inter_equal (Big.outer r) (Big.outer r')) &&
	  (Big.is_solid r) (* check that eta is total over r' sites *)

    let is_enabled b (r, _, _) =
      Big.occurs b r

		 (* Move to Big and add instantiation map on d *)
    let aux_apply (i_n, i_e, f_e) b r0 r1 =
      let (c, d, id) = decomp b r0 i_n i_e f_e in
      comp c (comp (tens r1 id) d)

	     (* Reduce a reducible class to the fixed point. Return the input state if no
     rewriting is performed. *)
    let fix s rules =
      let rec _step s = function
	| [] -> raise Big.NO_MATCH
	| (r, r', eta) :: rs -> (
	  try
	    aux_apply (Big.occurrence s r) s r r'
	  with
	  | Big.NO_MATCH -> _step s rs
	) in
      let rec _fix s rules i =
	try
	  _fix (_step s rules) rules (i + 1)
	with
	| Big.NO_MATCH -> (s, i) in
      _fix s rules 0

  end
		
		  
