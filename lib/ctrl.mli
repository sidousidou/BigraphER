(** The module of bigraphical controls. *)

type act = I of int | R of float
type param = A of act | F of string
type ide = string
type t = Ctrl of ide * act list | Fun_ctrl of ide * param list
(* val string_of_act : act -> string *)
(* val string_of_param : param -> string *)
(* val string_of_list : ('a -> string) -> 'a list -> string *)
val to_string : t -> string
val ide_compare : ide -> ide -> int
(* val int_compare : int -> int -> int *)
(* val float_compare : float -> float -> int *)
(* val act_compare : act -> act -> int *)
(* val param_compare : param -> param -> int *)
(* val list_compare : ('a -> 'b -> int) -> 'a list -> 'b list -> int *)
val compare : t -> t -> int
val ( = ) : t -> t -> bool
val actuals : t -> act list option
val norm : t -> t
val name : t -> ide
val parse : string -> t
