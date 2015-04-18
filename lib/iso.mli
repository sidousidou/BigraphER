(** This module provides an implementation of isomorphisms on
    integers. 
    @author Michele Sevegnani *)

type key = int
type +'a t
val empty : int t
val is_empty : int t -> bool
val mem : key -> int t -> bool

(* val singleton : key -> 'a -> 'a t *)
(* val remove : key -> 'a t -> 'a t *)
(* val merge : *)
(*   (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t *)

val iter : (key -> int -> unit) -> int t -> unit
val fold : (key -> int -> 'b -> 'b) -> int t -> 'b -> 'b

(* val for_all : (key -> 'a -> bool) -> 'a t -> bool *)
(* val exists : (key -> 'a -> bool) -> 'a t -> bool *)
(* val filter : (key -> 'a -> bool) -> 'a t -> 'a t *)
(* val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t *)

val cardinal : int t -> int

(* val bindings : 'a t -> (key * 'a) list *)
(* val min_binding : 'a t -> key * 'a *)
(* val max_binding : 'a t -> key * 'a *)
(* val choose : 'a t -> key * 'a *)
(* val split : key -> 'a t -> 'a t * 'a option * 'a t *)
(* val map : ('a -> 'b) -> 'a t -> 'b t *)

(* val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t *)

(** Compute the domain of an isomorphism. *)
val dom : int t -> key list

(** Compute the co-domain of an isomorphism. *)
val codom : int t -> int list

(** Return the inverse of an isomorphism. *)
val inverse : int t -> key t

exception NOT_BIJECTIVE

(** Add a new binding. Old binding on the same key is replaced.
    @raise NOT_BIJECTIVE if image is already in the isomorphism. *)
val add_exn : key -> int -> int t -> int t

val to_list : int t -> (key * int) list

(** [of_list l] returns an isomorphism with the elements in list [l]. 
    @raise NOT_BIJECTIVE sd *)
val of_list_exn : (key * int) list -> int t

val to_string : int t -> string

(** [is_id i] returns [true] if iso [i] is an identity, [false] otherwise.*)
val is_id : int t -> bool

val equal : int t -> int t -> bool

val compare : int t -> int t -> int

(** Union of two disjoint isomorphisms. 
    @raise NOT_BIJECTIVE asd *)
val union_exn : int t -> int t -> int t

(** Apply an iso to domain and one to codomain.
    @raise Not_found ads *)
val transform_exn : int t -> int t -> int t -> int t

(** Generate the equivalent isomorphisms by using a list of automorphisms. *)
val gen_isos_exn : int t -> key t list -> int t list

(** @raise Not_found ads*)
val find_exn : key -> int t -> int

val find : key -> int t -> int option
