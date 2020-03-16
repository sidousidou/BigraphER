(** This module provides an implementation of functions on integers.
    @author Hidden *)

(** Type of functions on integers. *)
type +'a t

(** {6 Standard map operations} *)	 
(** These functions are described in the {{:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html } standard
    library}. *)

val empty : int t
val is_empty : int t -> bool
val mem : int -> int t -> bool
val add : int -> int -> int t -> int t
val iter : (int -> int -> unit) -> int t -> unit
val fold : (int -> int -> 'b -> 'b) -> int t -> 'b -> 'b
val cardinal : int t -> int

(** {6 Additional functions} *)

(** Return the domain of a function. *)
val dom : int t -> IntSet.t

(** Return the codomain of a function. *)
val codom : int t -> IntSet.t

(** Return the inverse of a function. Note that the inverse of a function is, in
    the general case, a binary relation. *)
val inverse : int t -> IntSet.t Rel.t

(** Return the list of pairs defined by a function. *)
val to_list : int t -> (int * int) list

(** Inverse of {!Fun.to_list}. Note that in case of clashing pairs only the
    right-most is used. *)
val of_list : (int * int) list -> int t

(** [parse l] returns a function in which the numbers from [0] to [n - 1] (with
    [n] the length of [l]) are mapped to the elements of [l], in the given
    order. Example: [parse [0;0;3;1;2] = [(0,0);(1,0);(2,3);(3,1);(4,2)]]. *)
val parse : int list -> int t

(** Return the string representation of a function. Example: ["\{(1, 2), (2, 3),
    (3, 3)\}"]. *)
val to_string : int t -> string

(** Equality between functions. *)			   
val equal : int t -> int t -> bool

(** Comparison between functions. *)
val compare : int t -> int t -> int

(** [apply_exn f x] returns [f(x)]. 
    @raise Not_found if [f] is not defined for [x]. *)
val apply_exn : int t -> int -> int

(** Same as {!Fun.apply_exn} but with error-aware return type. *)				  
val apply : int t -> int -> int option

(** [transform_exn f iso_d iso_c] returns the function obtained by applying
    [iso_d] and [iso_c] to the domain and codomain of [f], respectively.
    @raise Not_found if the isomorphisms are undefined. *)
val transform_exn : int t -> int Iso.t -> int Iso.t -> int t
							   
(** Return [true] if a function is total, [false] otherwise. *)
val is_total : int -> int t -> bool

(** [check_codom min max f] returns [true] if the codomain of [f] is in the
    range [[min, max]]. *)
val check_codom : int -> int -> int t -> bool
				 
(**/**)
