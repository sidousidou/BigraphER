(** This module implements sets of integers.  
    @author Michele Sevegnani *)

(** Elements of a set. *)
type elt = int

(** The type of sets. *)	     
type t

(** {6 Standard set operations} *)	 
(** These functions are described the {{:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.Make.html } standard
    library}. *)
   
val empty : t
val is_empty : t -> bool
val mem : elt -> t -> bool
val add : elt -> t -> t
val singleton : elt -> t
val remove : elt -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val subset : t -> t -> bool
val iter : (elt -> unit) -> t -> unit
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
val for_all : (elt -> bool) -> t -> bool
val exists : (elt -> bool) -> t -> bool
val filter : (elt -> bool) -> t -> t
val partition : (elt -> bool) -> t -> t * t
val cardinal : t -> int
val elements : t -> elt list
val min_elt : t -> elt
val max_elt : t -> elt
val choose : t -> elt
val split : elt -> t -> t * bool * t

(** {6 Additional functions} *)
				     
(** Return the string representation of a set. Example: ["\{1,4,7\}"]. *)
val to_string : t -> string

(** Return a set containing the elements of a list. *)
val of_list : int list -> t

(** [of_int i] returns a set [{0, 1, ...., i - 1}]. *)
val of_int : int -> t

(** [off i s] adds offset [i] to all the elements of set [s]. *)
val off : int -> t -> t

(** Compute an isomorphism to fix the numbering of a set. For example, the
    isomorphism for set ["\{2, 5, 6, 7\}"] is ["\{(2,0), (5,1), (6,2),
    (7,3)\}"]. *)
val fix : t -> int Iso.t

(** Apply an isomorphism.
    @raise Not_found if a node identifier is not in the domain of the 
    isomorphism. *)
val apply_exn : t -> int Iso.t -> t

(** Compute the union of all the sets in a list. *)
val union_list : t list -> t
				    
(** Merge sets with common elements. *)
val merge : t list -> t list

(** Test if two sets are disjoint, {e i.e.} if their intersection is empty. *)
val disjoint : t -> t -> bool

(**/**)
