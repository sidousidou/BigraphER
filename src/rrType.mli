(** Type-dependent interface of rewrite rules. *)
module type R =
sig    
  type t
  type label
  type occ
  type edge
  val lhs : t -> Big.t
  val rhs : t -> Big.t
  val l : t -> label option
  val equal : t -> t -> bool
  val map : t -> Fun.t option
  val val_chk : t -> bool
  val val_chk_error_msg : string
  val string_of_label : label option -> string
  val parse : lhs:Big.t -> rhs:Big.t -> label -> Fun.t option -> t
  val to_occ : Big.t -> t -> occ
  val big_of_occ : occ -> Big.t
  val merge_occ : occ -> occ -> occ
  val update_occ : occ -> Big.t -> occ
  val edge_of_occ : occ -> int -> edge
  val step : Big.t -> t list -> occ list * int
  val random_step : Big.t -> t list -> occ option * int
end

module type T =
sig
  type t
  type label
  type occ
  type edge
  type react_error
  exception NOT_VALID of react_error
  val lhs : t -> Big.t
  val rhs : t -> Big.t
  val l : t -> label option
  val equal : t -> t -> bool
  val map : t -> Fun.t option
  val to_occ : Big.t -> t -> occ
  val parse : lhs:Big.t -> rhs:Big.t -> label -> Fun.t option -> t
  val big_of_occ : occ -> Big.t
  val merge_occ : occ -> occ -> occ
  val update_occ : occ -> Big.t -> occ
  val edge_of_occ : occ -> int -> edge
  val to_string : t -> string
  val is_valid : t -> bool
  val is_valid_exn : t -> bool			  
  val string_of_react_err : react_error -> string
  val is_enabled : Big.t -> t -> bool
  val apply : Big.t -> t list -> Big.t option
  val fix : Big.t -> t list -> Big.t * int
  val step : Big.t -> t list -> occ list * int
  val random_step : Big.t -> t list -> occ option * int
end

(** Module for the concrete implementation of basic operations on rewrite
    rules. *)
module Make (R : R) :
sig

  type t = R.t

  type label = R.label

  type occ = R.occ

  type edge = R.edge

  type react_error

  exception NOT_VALID of react_error

  (** Return the left-hand side of a rewrite rule. *)
  val lhs : t -> Big.t

  (** Return the right-hand side of a rewrite rule. *)
  val rhs : t -> Big.t

  (** Return the label of a rewrite rule if any. *)
  val l : t -> label option

  (** Eqaulity for reaction rules *)
  val equal : t -> t -> bool
  
  (** Return the instantition map of a rewrite rule. *)		   
  val map : t -> Fun.t option

  (** Creare a new reaction rule. *)
  val parse : lhs:Big.t -> rhs:Big.t -> label -> Fun.t option -> t

  (** Return an occurrence from a bigraph and a rewrite rule. *)		 
  val to_occ : Big.t -> t -> occ

  (** Return the bigraph component of an occurrence. *)				  
  val big_of_occ : occ -> Big.t

  (** Merge two occurrences. *)			 
  val merge_occ : occ -> occ -> occ

  (** [update_occ o b] returns a copy of [o] with [b] as bigraph. *)				    
  val update_occ : occ -> Big.t -> occ

  (** Replace the bigraph in an occurrence with an integer. *)					
  val edge_of_occ : occ -> int -> edge

  (** String representation of a rewrite rule. *)
  val to_string : t -> string

  (** Validity check. *)
  val is_valid : t -> bool

  (** Same as {!is_valid} but an exception is raised instead of returning
      [false].

      @raise NOT_VALID when the reaction rule is not valid. *)
  val is_valid_exn : t -> bool

  (** Convert to string the output of the validity check. *)
  val string_of_react_err : react_error -> string

  (** [is_enabled b r] checks if rewrite rule [r] can be applied to bigraph
      [b]. *)
  val is_enabled : Big.t -> t -> bool

  (** Apply a list of reaction rules in sequence. Non-enabled rules are
      ignored. *)
  val apply : Big.t -> t list -> Big.t option
                                    
  (** [fix b r_list] applies the rewrite rules in list [r_list] to bigraph [b]
      until a fixed point [b'] is reached. The result is fixed point [b'] and
      the number of rewriting steps performed. Note, [b] is returned when no
      rewriting is performed. *)
  val fix : Big.t -> t list -> Big.t * int

  (** All the possible evolutions in one step. Total number of occurrences also
      returned. *)
  val step : Big.t -> t list -> occ list * int

  (** Random step. *)
  val random_step : Big.t -> t list -> occ option * int
  
end

(** Generic step function *)
val gen_step :
  Big.t ->
  'b list ->
  big_of_occ:('a -> Big.t) ->
  to_occ:(Big.t -> 'b -> 'a) ->
  merge_occ:('a -> 'a -> 'a) ->
  lhs:('b -> Big.t) ->
  rhs:('b -> Big.t) ->
  map:('b -> Fun.t option) ->
  'a list * int
                                                                    
(**/**)
