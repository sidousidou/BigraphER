(** This module provides an implementation of isomorphisms on integers.
    @author Michele Sevegnani *)

(** Type of isomorphisms on integers. *)
type +'a t

(** Isomorphisms are bijective functions. For example, pairs 

    ["\{(1, 2), (3, 2)\}"]

    are not allowed because value [2] in the co-domain is the mapping of both
    [1] and [3]. This exception is raised when an invalid isomorphism is
    generated. *)
exception NOT_BIJECTIVE

(** {3 Standard map operations} *)	 
(** These functions are described in the {{:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.Make.html } standard
    library}. *)

val empty : int t
val is_empty : int t -> bool
val mem : int -> int t -> bool
val iter : (int -> int -> unit) -> int t -> unit
val fold : (int -> int -> 'b -> 'b) -> int t -> 'b -> 'b
val cardinal : int t -> int

(** {3 Additional functions} *)

(** Return the domain of an isomorphism. *)
val dom : int t -> int list

(** Return the co-domain of an isomorphism. *)
val codom : int t -> int list

(** Return the inverse of an isomorphism. *)
val inverse : int t -> int t

(** Return the list of pairs defined by an isomorphism. *)			   
val to_list : int t -> (int * int) list

(** [of_list l] returns an isomorphism with the elements in list [l]. In case of
    conflicting pairs, the right-most is added to the isomorphism.  
    @raise NOT_BIJECTIVE if [l] is not a valid isomorphism. *)
val of_list_exn : (int * int) list -> int t

(** Return the string representation of an isomorphism. Example:

    ["\{(1, 1), (2, 3), (5, 2) \}"]. *)
val to_string : int t -> string

(** [add_exn i j iso] adds a new pair [(i, j)] to isomorphism [iso]. If [i] was
    already bound in [iso], its previous binding disappears.

    @raise NOT_BIJECTIVE when [j] is already in the co-domain of [iso]. *)
val add_exn : int -> int -> int t -> int t

(** Equality between isomorphisms. *)		       
val equal : int t -> int t -> bool

(** Comparison between isomorphisms. *)				
val compare : int t -> int t -> int

(** [apply_exn iso i] returns the value associated to [i] by isomorphism [iso].
    @raise Not_found if [iso] is undefined for [i]. *)
val apply_exn : int t -> int -> int

(** Same as {!Iso.apply_exn} but with error-aware return type. *)
val apply : int t -> int -> int option

(** [transform_exn i iso_d iso_c] returns the isomorphism obtained by
    applying [iso_d] and [iso_c] to the domain and co-domain of [i],
    respectively.  
    @raise Not_found if the isomorphisms are undefined. *)
val transform_exn : int t -> int t -> int t -> int t

(** [gen_isos_exn iso autos] generates the symmetric isomorphisms of [iso] by
    using [autos], a list of automorphisms over the domain of [iso]. This is the
    same as iterating {!Iso.transform_exn} over [auto], use each automorphism as
    domain isomorphism and the identity as co-domain isomorphism.    
    @raise Not_found if some automorphism is undefined. *)
val gen_isos_exn : int t -> int t list -> int t list

(** Returns [true] if an isomorphism is an identity, [false] otherwise. *)
val is_id : int t -> bool

(**/**)
