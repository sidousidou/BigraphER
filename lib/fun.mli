(** This module provides an implementation of finite functions over integers.

    @author Michele Sevegnani *)

(** Type of finite functions over integers. *)
type t

(** {3 Standard map operations} *)	 
(** These functions are described in the {{:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html } standard
    library}. *)

val add : int -> int -> t -> t
val compare : t -> t -> int
val empty : t
val equal : t -> t -> bool
val fold : (int -> int -> 'b -> 'b) -> t -> 'b -> 'b

(** {3 Additional functions} *)

(** Return the domain of a function. *)
val dom : t -> IntSet.t

(** Return the codomain of a function. *)
val codom : t -> IntSet.t

(** Return the inverse of a function. Note that the inverse of a function is, in
    the general case, a binary relation. *)
val inverse : t -> Rel.t

(** Return the list of pairs defined by a function. *)
val to_list : t -> (int * int) list

(** Inverse of {!val:Fun.to_list}. Note that in case of clashing pairs only the
    right-most is used. *)
val of_list : (int * int) list -> t

(** [parse l] returns a function in which the numbers from [0] to [n - 1] (with
    [n] the length of [l]) are mapped to the elements of [l], in the given
    order. Example:

    [parse [0;0;3;1;2] = [(0,0);(1,0);(2,3);(3,1);(4,2)]]. *)
val parse : int list -> t

(** Return the string representation of a function. Example: 

    ["\{(1, 2), (2, 3), (3, 3)\}"]. *)
val to_string : t -> string

(** [apply f x] returns [f(x)]. *)				  
val apply : t -> int -> int option

(** [transform ~iso_dom ~iso_codom f] returns the function obtained by applying
    [iso_dom] and [iso_codom] to the domain and codomain of [f],
    respectively. *)
val transform : iso_dom:Iso.t -> iso_codom:Iso.t -> t -> t

(** [is_total n f] returns [true] if function [f] is total over domain
    [1,...,n-1], [false] otherwise. *)
val is_total : int -> t -> bool

(** [check_codom ~min ~max f] returns [true] if the codomain of [f] is in the
    range [[min, max]]. *)
val check_codom : min:int -> max:int -> t -> bool

(**/**)
