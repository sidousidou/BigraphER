(** This module provides an implementation of isomorphisms on integers.
    @author Michele Sevegnani *)

type key = int
type +'a t
val empty : int t
val is_empty : int t -> bool
val mem : key -> int t -> bool
val add : key -> int -> int t -> int t
val singleton : key -> int -> int t
val remove : key -> int t -> int t
(*val merge :
  (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t*)
val iter : (key -> int -> unit) -> int t -> unit
val fold : (key -> int -> 'b -> 'b) -> int t -> 'b -> 'b
val for_all : (key -> int -> bool) -> int t -> bool
val exists : (key -> int -> bool) -> int t -> bool
val filter : (key -> int -> bool) -> int t -> int t
val partition : (key -> int -> bool) -> int t -> int t * int t
val cardinal : int t -> int
val bindings : int t -> (key * int) list
val min_binding : int t -> key * int
val max_binding : int t -> key * int
val choose : int t -> key * int
val split : key -> int t -> int t * int option * int t
val find : key -> int t -> int
val map : (int -> 'b) -> int t -> 'b t
(*val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t*)

(** Return the inverse of an isomorphism. *)
val inverse : int t -> key t

(** Compute the domain of an isomorphism. *)
val dom : int t -> key list

(** Compute the co-domain of an isomorphism. *)
val codom : int t -> int list

val to_list : int t -> (key * int) list

(** [of_list l] returns an isomorphism with the elements in list [l]. *)
val of_list : (key * int) list -> int t

val to_string : int t -> string

(** [is_id i] returns [true] if iso [i] is an identity, [false] otherwise.*)
val is_id : int t -> bool

val equal : int t -> int t -> bool

val compare : int t -> int t -> int

val union : int t -> int t -> int t

(** Apply an iso to domain and one to codomain.
    @raise Not_found *)
val transform : int t -> int t -> int t -> int t

(** Generate the equivalent isomorphisms by using a list of automorphisms. *)
val gen_isos : int t -> key t list -> int t list
