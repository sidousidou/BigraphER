(** This module provides an implementation of fintite binary relations over
    integers.

    @author Michele Sevegnani *)

type t
(** Type of finite binary relations on integers. *)

(** {2 Standard map operations} *)

(** These functions are described in the
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html}
    standard library}. *)

val cardinal : t -> int

val empty : t

val fold : (int -> IntSet.t -> 'b -> 'b) -> t -> 'b -> 'b

val is_empty : t -> bool

val iter : (int -> IntSet.t -> unit) -> t -> unit

val mem : int -> t -> bool

(** {2 Additional functions} *)

val dom : t -> IntSet.t
(** Return the domain of a binary relation. *)

val codom : t -> IntSet.t
(** Return the codomain of a binary relation. *)

val to_list : t -> (int * IntSet.t) list
(** Return the list of pairs defined by a binary relation. *)

val of_list : (int * int list) list -> t
(** Inverse of {!Rel.to_list}. Note that in case of clashing pairs only the
    right-most is used. *)

val to_string : t -> string
(** Return the string representation of a binary relation. Example:

    {[ {(1, {1, 2, 3}), (2, {3, 4})} ]} *)

val pp : Format.formatter -> t -> unit
(** Pretty printer. *)

val add : int -> IntSet.t -> t -> t
(** [add i set r] adds [set] to the values associated to [i] in relation [r]. *)

val equal : t -> t -> bool
(** Equality between binary relations. *)

val compare : t -> t -> int
(** Comparison between binary relations. *)

val apply : t -> int -> IntSet.t
(** [applyr i] returns the set of values associated to [i] by relation [r]. *)

(**/**)
