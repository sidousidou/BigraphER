type key = int
type +'a t
val empty : int t
val is_empty : int t -> bool
(* val mem : key -> int t -> bool *)
val add : key -> int -> int t -> int t
val singleton : key -> int -> int t
(* val remove : key -> int t -> int t *)
(* val merge : *)
(*   (key -> 'a option -> 'b option -> 'c option) -> int t -> 'b t -> 'c t *)
val compare : (int -> int -> int) -> int t -> int t -> int
val equal : (int -> int -> bool) -> int t -> int t -> bool
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
(* val split : key -> int t -> int t * 'a option * int t *)
val find : key -> int t -> int
val map : (int -> 'b) -> int t -> 'b t
val mapi : (key -> int -> 'b) -> int t -> 'b t
val to_string : int t -> string
val arities : int t -> (key * int) list
val of_nodes : Ctrl.t Node.t -> int Sig.t -> int t
val to_IntSet : int t -> IntSet.t
val apply_exn : int t -> int Iso.t -> int t
val apply : int t -> int Iso.t -> int t
(* Multiset union *)
val union : int t -> int t -> int t
val subset : int t -> int t -> Ctrl.t Node.t -> Ctrl.t Node.t -> bool
val types : int t -> Ctrl.t Node.t -> String.t list
val compat_list :
  int t -> int t -> Ctrl.t Node.t -> Ctrl.t Node.t -> Cnf.lit list list
val offset : int t -> int -> int t
