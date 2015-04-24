(** Type-dependent interface of rewrite rules. *)
module type R =
  sig
    
    (** The type of a rewrite rule. *)
    type t

    (** The type of a label. *)
    type label
	   
    (** The type of an occurrence. *)
    type occ
	   
    (** Return the left-hand side of a rewrite rule. *)
    val lhs : t -> Big.bg

    (** Return the right-hand side of a rewrite rule. *)
    val rhs : t -> Big.bg

    (** Return the label of a rewrite rule. *)
    val l : t -> label
		   
    (** Return the string representation of a label. *)
    val string_of_label : label -> string

    (** Return the instantition map of a rewrite rule. *)		   
    val map : t -> int Fun.t option
				     
    (** Return [true] if a rewrite rule is valid, [false] otherwise. This
        function only performs the additional type-dependent checks. *)
    val val_chk : t -> bool

    (** Return an occurrence from a bigraph and a rewrite rule. *)		 
    val to_occ : Big.bg -> t -> occ

    (** Return the bigraph component of an occurrence. *)				  
    val big_of_occ : occ -> Big.bg
			      
    (** Merge two occurrences. *)			 
    val merge_occ : occ -> occ -> occ
			 
  end

(** Common interface of concrete rewrite rules. *)		  
module type T =
  sig

    type t

    type label
	   
    type occ

    (** Return the left-hand side of a rewrite rule. *)
    val lhs : t -> Big.bg

    (** Return the right-hand side of a rewrite rule. *)
    val rhs : t -> Big.bg

    (** Return the label of a rewrite rule. *)
    val l : t -> label
		   
    (** Return the instantition map of a rewrite rule. *)		   
    val map : t -> int Fun.t option

    (** String representation of a rewrite rule. *)
    val to_string : t -> string

    (** Validity check. *)
    val is_valid : t -> bool

    (** [is_enabled b r] checks if rewrite rule [r] can be applied to bigraph
        [b]. *)
    val is_enabled : Big.bg -> t -> bool

    (** [fix b r_list] applies the rewrite rules in list [r_list] to bigraph [b]
        until a fixed point [b'] is reached. The result is fixed point [b'] and
        the number of rewriting steps performed. Note, [b] is returned when no
        rewriting is performed. *)
    val fix : Big.bg -> t list -> Big.bg * int

    (** All the possible evolutions in one step. Total number of occurrences also
      returned. *)
    val step : Big.bg -> t list -> occ list * int

  end

    
(** Module for the concrete implementation of basic operations on rewrite
    rules. *)
module Make :
functor (R : R) ->
sig
  type t = R.t
  type label = R.label
  type occ = R.occ
  val lhs : t -> Big.bg
  val rhs : t -> Big.bg
  val l : t -> label
  val map : t -> int Fun.t option
  val to_string : t -> string
  val is_valid : t -> bool
  val is_enabled : Big.bg -> t -> bool
  val fix : Big.bg -> t list -> Big.bg * int
  val step : Big.bg -> t list -> occ list * int
end

(* module Make (R : R) : T with type t = R.t and type label = R.label and type occ = R.occ *)
