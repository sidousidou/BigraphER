(** This module implements finite sets of integers.  

    @author Michele Sevegnani *)

(** The type of sets of integers. *)	     
type t

(** {3 Standard set operations} *)	 
(** These functions are described in the {{:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.Make.html } standard
    library}. *)

val add : int -> t -> t
val cardinal : t -> int
val compare : t -> t -> int
val diff : t -> t -> t
val elements : t -> int list
val empty : t
val equal : t -> t -> bool
val exists : (int -> bool) -> t -> bool
val filter : (int -> bool) -> t -> t
val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a
val for_all : (int -> bool) -> t -> bool
val inter : t -> t -> t
val is_empty : t -> bool
val iter : (int -> unit) -> t -> unit
val max_elt : t -> int option
val mem : int -> t -> bool
val min_elt : t -> int option
val partition : (int -> bool) -> t -> t * t
val remove : int -> t -> t
val singleton : int -> t
val subset : t -> t -> bool
val union : t -> t -> t

(** {3 Additional functions} *)

(** Return the string representation of a set. Example: ["\{1,4,7\}"]. *)
val to_string : t -> string

(** Return a set containing the elements of a list. *)
val of_list : int list -> t

(** [of_int i] returns a set [{0, 1, ...., i - 1}]. *)
val of_int : int -> t

(** [off i s] adds offset [i] to all the elements of set [s]. *)
val off : int -> t -> t

(** Compute an isomorphism to fix the numbering of a set. For example, the
    isomorphism for set ["\{2, 5, 6, 7\}"] is 
    ["\{(2,0), (5,1), (6,2), (7,3)\}"]. *)
val fix : t -> Iso.t

(** [apply iso s] applies [iso] to each element of [s]. Elements not mapped by
    [iso] are ignored. *)
val apply : Iso.t -> t -> t

(** Compute the union of all the sets in a list. *)
val union_list : t list -> t

(** Merge sets with common elements. *)
val merge : t list -> t list

(** Test if two sets are disjoint, {e i.e.} if their intersection is empty. *)
val disjoint : t -> t -> bool

(**/**)
