(** This module provides operations for sets of int.  
    @author Michele Sevegnani *)

type elt = int
type t
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

(** [to_string s] gives the string representation of [Int_set s].*)
val to_string : t -> string

(** [of_list l] returns a set of int form a list *)
val of_list : int list -> t

(** Compute the union of all the sets in the input list. *)
val union_list : t list -> t

(** [of_int i] returns a set [{0, 1, ...., i-1}].*)
val of_int: int -> t

(** [off i s] adds offset [i] to all the elements of set [s].*)
val off: int -> t -> t

(** [norm s] normalises set [s]: e.g. [{4, 6, 7, 9} --> {0, 1, 2, 3}] *)
val norm : t -> t

(** [fix s] generates an isomorphism to fix the numbering of [s]: e.g.
    [{2, 5, 6, 7} --> {(2,0), (5,1), (6,2), (7,3)}]*)
val fix : t -> int Iso.t

(** Apply an isomorphism *)
val apply_exn : t -> int Iso.t -> t

(** Merge sets with common elements. *)
val merge : t list -> t list

(** Check if the intersection of two sets is empty. *)
val disjoint : t -> t -> bool
