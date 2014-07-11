(** The module of bigraphical signatures. 
    A control ide is associated to an arity and a list of types *)

(* type arg_type = *)
(*   [ `int *)
(*   | `float *)
(*   | `unknown ] *)

(* type types = arg_type list *)

type key = Ctrl.ide
type +'a t

val empty : int t

(* Old binding is replaced *)
val add : key -> int -> int t -> int t

val iter : (key -> int -> unit) -> int t -> unit
val fold : (key -> int -> 'b -> 'b) -> int t -> 'b -> 'b
val arity : key -> int t -> int option
(* val string_of_types :  types -> string *)
