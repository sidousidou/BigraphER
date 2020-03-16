(** This module provides an implementation of functions on integers.
    @author Michele Sevegnani *)

type key = int
type +'a t
val empty : int t
val is_empty : int t -> bool
val mem : key -> int t -> bool
val add : key -> int -> int t -> int t

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

val dom : int t -> key list
val codom : int t -> int list
val inverse : int t -> IntSet.t Rel.t
val to_list : int t -> (key * int) list
val of_list : (key * int) list -> int t
val to_string : int t -> string
val equal : int t -> int t -> bool
val compare : int t -> int t -> int

val union : int t -> int t -> int t
val transform_exn : int t -> int Iso.t -> int Iso.t -> int t
val find_exn : key -> int t -> int
val find : key -> int t -> int option
val is_total : key -> int t -> bool
