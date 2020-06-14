(** This module provides an implementation of finite functions over integers.

    @author Michele Sevegnani *)

type t
(** Type of finite functions over integers. *)

(** {2 Standard map operations} *)

(** These functions are described in the
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html}
    standard library}. *)

val add : int -> int -> t -> t

val compare : t -> t -> int

val empty : t

val equal : t -> t -> bool

val fold : (int -> int -> 'b -> 'b) -> t -> 'b -> 'b

val iter : (int -> int -> unit) -> t -> unit

(** {2 Additional functions} *)

val dom : t -> IntSet.t
(** Return the domain of a function. *)

val codom : t -> IntSet.t
(** Return the codomain of a function. *)

val inverse : t -> Rel.t
(** Return the inverse of a function. Note that the inverse of a function is,
    in the general case, a binary relation. *)

val to_list : t -> (int * int) list
(** Return the list of pairs defined by a function. *)

val of_list : (int * int) list -> t
(** Inverse of {!val:Fun.to_list}. Note that in case of clashing pairs only
    the right-most is used. *)

val parse : int list -> t
(** [parse l] returns a function in which the numbers from [0] to [n - 1]
    (with [n] the length of [l]) are mapped to the elements of [l], in the
    given order. Example:

    [parse \[0;0;3;1;2\] = \[(0,0);(1,0);(2,3);(3,1);(4,2)\]]. *)

val to_string : t -> string
(** Return the string representation of a function. Example:

    {[ {(1, 2), (2, 3), (3, 3)} ]} *)

val pp : Format.formatter -> t -> unit
(** Pretty printer. *)

val apply : t -> int -> int option
(** [apply f x] returns [f(x)]. *)

val transform : iso_dom:Iso.t -> iso_codom:Iso.t -> t -> t
(** [transform ~iso_dom ~iso_codom f] returns the function obtained by
    applying [iso_dom] and [iso_codom] to the domain and codomain of [f],
    respectively. *)

val is_total : int -> t -> bool
(** [is_total n f] returns [true] if function [f] is total over domain
    [0,...,n-1], [false] otherwise. *)

val is_surj : int -> t -> bool
(** [is_surj n f] returns [true] if function [f] is surjective over codomain
    [0,...,n-1], [false] otherwise. *)

val is_id : t -> bool
(** [is_id f] returns [true] if [f(i) = i] for each [i], [false] otherwise. *)

val check_codom : int -> t -> bool
(** [check_codom n f] returns [true] if the codomain of [f] is in the range
    [\[0, n-1\]]. *)

(**/**)
