(** This module provides an implementation of binary relations on integers.
    @author Michele Sevegnani *)

type key = int
type +'a t
val empty : IntSet.t t
val is_empty : IntSet.t t -> bool
val mem : key -> IntSet.t t -> bool

(* val singleton : key -> 'a -> 'a t *)
(* val remove : key -> 'a t -> 'a t *)
(* val merge : *)
(*   (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t *)

val iter : (key -> IntSet.t -> unit) -> IntSet.t t -> unit
val fold : (key -> IntSet.t -> 'b -> 'b) -> IntSet.t t -> 'b -> 'b

(* val for_all : (key -> 'a -> bool) -> 'a t -> bool *)
(* val exists : (key -> 'a -> bool) -> 'a t -> bool *)
(* val filter : (key -> 'a -> bool) -> 'a t -> 'a t *)
(* val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t *)

val cardinal : IntSet.t t -> int

(* val bindings : 'a t -> (key * 'a) list *)
(* val min_binding : 'a t -> key * 'a *)
(* val max_binding : 'a t -> key * 'a *)
(* val choose : 'a t -> key * 'a *)
(* val split : key -> 'a t -> 'a t * 'a option * 'a t *)
(* val map : ('a -> 'b) -> IntSet.t t -> 'b t *)
(* val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t *)

val find_exn : key -> IntSet.t t -> IntSet.t
val find : key -> IntSet.t t -> IntSet.t

val add : key -> IntSet.t -> IntSet.t t -> IntSet.t t
val dom : IntSet.t t -> key list
val codom : IntSet.t t -> IntSet.t
val is_fun : IntSet.t t -> bool
val equal : IntSet.t t -> IntSet.t t -> bool
val compare : IntSet.t t -> IntSet.t t -> int
val to_string : IntSet.t t -> string
val to_list : IntSet.t t -> (key * IntSet.t) list
val of_list : (key * int list) list -> IntSet.t t
val union : IntSet.t t -> IntSet.t t -> IntSet.t t
val inverse : IntSet.t t -> IntSet.t t
val transform_exn : IntSet.t t -> int Iso.t -> int Iso.t -> IntSet.t t
