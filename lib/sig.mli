(** The module of bigraphical signatures. *)
type key = Ctrl.ide
type +'a t
val empty : int t
(* val is_empty : 'a t -> bool *)
(* val mem : key -> 'a t -> bool *)
val add : key -> int -> int t -> int t
(* val singleton : key -> 'a -> 'a t *)
(* val remove : key -> 'a t -> 'a t *)
(* val merge : *)
(*   (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t *)
(* val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int *)
(* val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool *)
val iter : (key -> int -> unit) -> int t -> unit
val fold : (key -> int -> 'b -> 'b) -> int t -> 'b -> 'b
(* val for_all : (key -> 'a -> bool) -> 'a t -> bool *)
(* val exists : (key -> 'a -> bool) -> 'a t -> bool *)
(* val filter : (key -> 'a -> bool) -> 'a t -> 'a t *)
(* val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t *)
(* val cardinal : 'a t -> int *)
(* val bindings : 'a t -> (key * 'a) list *)
(* val min_binding : 'a t -> key * 'a *)
(* val max_binding : 'a t -> key * 'a *)
(* val choose : 'a t -> key * 'a *)
(* val split : key -> 'a t -> 'a t * 'a option * 'a t *)
(* val find : key -> 'a t -> 'a *)
(* val map : ('a -> 'b) -> 'a t -> 'b t *)
(* val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t *)
val arity : key -> int t -> int option
