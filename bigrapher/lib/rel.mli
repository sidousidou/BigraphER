(** This module provides an implementation of binary relations on integers.
    @author Michele Sevegnani *)

(** Type of binary relations on integers. *)
type +'a t

(** {3 Standard map operations} *)	 
(** These functions are described in the {{:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html } standard
    library}. *)

val empty : IntSet.t t
val is_empty : IntSet.t t -> bool
val mem : int -> IntSet.t t -> bool
val iter : (int -> IntSet.t -> unit) -> IntSet.t t -> unit
val fold : (int -> IntSet.t -> 'b -> 'b) -> IntSet.t t -> 'b -> 'b
val cardinal : IntSet.t t -> int

(** {3 Additional functions} *)

(** Return the domain of a binary relation. *)
val dom : IntSet.t t -> int list

(** Return the codomain of a binary relation. *)			    
val codom : IntSet.t t -> IntSet.t

(** Return the inverse of a binary relation. *)			    
val inverse : IntSet.t t -> IntSet.t t

(** Return the list of pairs defined by a binary relation. *)
val to_list : IntSet.t t -> (int * IntSet.t) list

(** Inverse of {!Rel.to_list}. Note that in case of clashing pairs only the
    right-most is used. *)					     
val of_list : (int * int list) list -> IntSet.t t

(** Return the string representation of a binary relation. Example: ["\{(1, \{1,
    2, 3\}), (2, \{3, 4\})\}"]. *)
val to_string : IntSet.t t -> string

(** Equality between binary relations. *)
val equal : IntSet.t t -> IntSet.t t -> bool

(** Comparison between binary relations. *)		  
val compare : IntSet.t t -> IntSet.t t -> int

(** [add i set r] adds [set] to the values associated to [i] in relation [r]. *)
val add : int -> IntSet.t -> IntSet.t t -> IntSet.t t

(** [apply_exn r i] returns the set of values associated to [i] by relation [r]. 
    @raise Not_found if [r] is undefined for [i]. *)
val apply_exn : IntSet.t t -> int -> IntSet.t

(** Same as {!Rel.apply_exn} but the empty set is returned on error. *)
val apply : IntSet.t t -> int -> IntSet.t

(** [transform_exn r iso_d iso_c] returns the binary relation obtained by
    applying [iso_d] and [iso_c] to the domain and codomain of [r],
    respectively.  
    @raise Not_found if the isomorphisms are undefined. *)
val transform_exn : IntSet.t t -> int Iso.t -> int Iso.t -> IntSet.t t

(** Return [true] if a binary relation is a function, [false] otherwise. *)
val is_fun : IntSet.t t -> bool

(**/**)
