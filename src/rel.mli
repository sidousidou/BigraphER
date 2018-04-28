(** This module provides an implementation of fintite binary relations over
    integers.
    @author Michele Sevegnani *)

(** Type of finite binary relations on integers. *)
type t

(** {2 Standard map operations} *)	 
(** These functions are described in the {{:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html } standard
    library}. *)

val cardinal : t -> int
val empty : t
val fold : (int -> IntSet.t -> 'b -> 'b) -> t -> 'b -> 'b
val is_empty : t -> bool
val iter : (int -> IntSet.t -> unit) -> t -> unit
val mem : int -> t -> bool

(** {2 Additional functions} *)

(** Return the domain of a binary relation. *)
val dom : t -> IntSet.t

(** Return the codomain of a binary relation. *)			    
val codom : t -> IntSet.t

(** Return the list of pairs defined by a binary relation. *)
val to_list : t -> (int * IntSet.t) list

(** Inverse of {!Rel.to_list}. Note that in case of clashing pairs only the
    right-most is used. *)					     
val of_list : (int * int list) list -> t

(** Return the string representation of a binary relation. Example: 
    
    ["\{(1, \{1, 2, 3\}), (2, \{3, 4\})\}"]. *)
val to_string : t -> string

(** Pretty printer. *)
val pp : Format.formatter -> t -> unit
  
(** [add i set r] adds [set] to the values associated to [i] in relation [r]. *)
val add : int -> IntSet.t -> t -> t

(** Equality between binary relations. *)
val equal : t -> t -> bool

(** Comparison between binary relations. *)		  
val compare : t -> t -> int

(** [applyr i] returns the set of values associated to [i] by relation [r]. *)
val apply : t -> int -> IntSet.t

(**/**)
