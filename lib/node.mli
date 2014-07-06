(** Module providing an implementation for the nodes of a bigraph.
    @author Michele Sevegnani *)

type key = int
type +'a t 
val empty : Ctrl.t t
val is_empty : Ctrl.t t -> bool
val mem : key -> Ctrl.t t -> bool
val add : key -> Ctrl.t -> Ctrl.t t -> Ctrl.t t
val singleton : key -> Ctrl.t -> Ctrl.t t
val remove : key -> Ctrl.t t -> Ctrl.t t
(* val merge : *)
(*   (key -> Ctrl.t option -> 'b option -> 'c option) -> Ctrl.t t -> 'b t -> 'c t *)
(* val compare : (Ctrl.t -> Ctrl.t -> int) -> Ctrl.t t -> Ctrl.t t -> int *)
val iter : (key -> Ctrl.t -> unit) -> Ctrl.t t -> unit
val fold : (key -> Ctrl.t -> 'b -> 'b) -> Ctrl.t t -> 'b -> 'b
val for_all : (key -> Ctrl.t -> bool) -> Ctrl.t t -> bool
val exists : (key -> Ctrl.t -> bool) -> Ctrl.t t -> bool
val filter : (key -> Ctrl.t -> bool) -> Ctrl.t t -> Ctrl.t t
val partition : (key -> Ctrl.t -> bool) -> Ctrl.t t -> Ctrl.t t * Ctrl.t t
val cardinal : Ctrl.t t -> int
val bindings : Ctrl.t t -> (key * Ctrl.t) list
(* val min_binding : Ctrl.t t -> key * Ctrl.t *)
(* val max_binding : Ctrl.t t -> key * Ctrl.t *)
(* val choose : Ctrl.t t -> key * Ctrl.t *)
(* val split : key -> Ctrl.t t -> Ctrl.t t * Ctrl.t option * Ctrl.t t *)
val find_exn : key -> Ctrl.t t -> Ctrl.t
val ctrl: key -> Ctrl.t t -> Ctrl.t option
val map : (Ctrl.t -> 'b) -> Ctrl.t t -> 'b t
(* val mapi : (key -> Ctrl.t -> 'b) -> Ctrl.t t -> 'b t *)
val to_string : Ctrl.t t -> string
val to_dot : Ctrl.t t -> string
val tens : Ctrl.t t -> Ctrl.t t -> int -> Ctrl.t t
val apply_exn : Ctrl.t t -> int Iso.t -> Ctrl.t t
val filter_apply_iso : Ctrl.t t -> int Iso.t -> Ctrl.t t
val not_sub : Ctrl.t t -> Ctrl.t t -> bool
val norm : Ctrl.t t -> Ctrl.t list
val equal : Ctrl.t t -> Ctrl.t t -> bool
val same_ctrl : Ctrl.t -> Ctrl.t t -> IntSet.t
val parse : string -> Ctrl.t t
val apply_subs : Ctrl.t t -> (Ctrl.ide * Ctrl.act) list -> Ctrl.t t
val is_fun : Ctrl.t t -> bool
val actuals : Ctrl.t t -> Ctrl.act list
