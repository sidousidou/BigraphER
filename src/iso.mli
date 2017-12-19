(** This module provides an implementation of finite isomorphisms over integers.
    @author Michele Sevegnani *)

(** Type of isomorphisms over integers. *)
type t

(** {3 Standard map operations} *)	 
(** These functions are described in the {{:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html } standard
    library}. *)

val cardinal : t -> int
val empty : t
val fold : (int -> int -> 'b -> 'b) -> t -> 'b -> 'b
val is_empty : t -> bool
val iter : (int -> int -> unit) -> t -> unit
val mem : int -> t -> bool

(** {3 Additional functions} *)

(** Return the domain of an isomorphism. *)
val dom : t -> int list

(** Return the co-domain of an isomorphism. *)
val codom : t -> int list

(** Return the inverse of an isomorphism. *)
val inverse : t -> t

(** [add i j iso] adds a new pair [(i, j)] to isomorphism [iso]. If [i] or [j]
    are already bound in [iso], [iso] is returned unmodified. *)
val add : int -> int -> t -> t

(** Return the list of pairs defined by an isomorphism. *)			   
val to_list : t -> (int * int) list

(** [of_list l] returns an isomorphism with the elements in list [l]. In case of
    conflicting pairs, the left-most is added to the isomorphism. *)
val of_list : (int * int) list -> t

(** Return the string representation of an isomorphism. Example:

    ["\{(1, 1), (2, 3), (5, 2)\}"]. *)
val to_string : t -> string

(** Equality between isomorphisms. *)		       
val equal : t -> t -> bool

(** Comparison between isomorphisms. *)				
val compare : t -> t -> int

(** [apply iso i] returns the value associated to [i] by isomorphism [iso]. *)
val apply : t -> int -> int option

(** [transform ~iso_dom ~iso_codom i] returns the isomorphism obtained by
    applying [iso_dom] and [iso_codom] to the domain and co-domain of [i],
    respectively. Entries in [i] that cannot be mapped by either [iso_dom] or
    [iso_codom] are ignored. *)
val transform : iso_dom:t -> iso_codom:t -> t -> t

(** [gen_isos iso autos] generates the symmetric isomorphisms of [iso] by using
    [autos], a list of automorphisms over the domain of [iso]. Entries in [iso]
    not mapped by an automorphism are ignored. *)
val gen_isos : t -> t list -> t list

(** Returns [true] if an isomorphism is an identity, [false] otherwise. *)
val is_id : t -> bool

(**/**)
