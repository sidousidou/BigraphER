(** This module provides an implementation of isomorphisms on integers.
    @author Michele Sevegnani *)

type key = int
type +'a t
val empty : 'a t
val is_empty : 'a t -> bool
val mem : key -> 'a t -> bool
val add : key -> 'a -> 'a t -> 'a t
val singleton : key -> 'a -> 'a t
val remove : key -> 'a t -> 'a t
(*val merge :
  (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t*)
val iter : (key -> 'a -> unit) -> 'a t -> unit
val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val for_all : (key -> 'a -> bool) -> 'a t -> bool
val exists : (key -> 'a -> bool) -> 'a t -> bool
val filter : (key -> 'a -> bool) -> 'a t -> 'a t
val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
val cardinal : 'a t -> int
val bindings : 'a t -> (key * 'a) list
val min_binding : 'a t -> key * 'a
val max_binding : 'a t -> key * 'a
val choose : 'a t -> key * 'a
val split : key -> 'a t -> 'a t * 'a option * 'a t
val find : key -> 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
(*val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t*)

(** Return the inverse of an isomorphism. *)
val inverse : key t -> key t

(** Compute the domain of an isomorphism. *)
val dom : 'a t -> key list

(** Compute the co-domain of an isomorphism. *)
val codom : 'a t -> 'a list

val to_list : 'a t -> (key * 'a) list

(** [of_list l] returns an isomorphism with the elements in list [l]. *)
val of_list : (key * 'a) list -> 'a t

val to_string : int t -> string

(** [is_id i] returns [true] if iso [i] is an identity, [false] otherwise.*)
val is_id : key t -> bool

val equal : '_a t -> '_a t -> bool

val compare : int t -> int t -> int

val union : '_a t -> '_a t -> '_a t

(** Apply an iso to domain and one to codomain.
    @raise Not_found *)
val transform : key t -> key t -> 'a t -> 'a t

(** Generate the equivalent isomorphisms by using a list of automorphisms. *)
val gen_isos : 'a t -> key t list -> 'a t list
