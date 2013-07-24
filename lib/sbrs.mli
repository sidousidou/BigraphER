(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on BRS.
@author Michele Sevegnani *)

(** The type of bigraphical reaction rules.*)
type react = {
  rdx : Big.bg; (** Redex *)
  rct : Big.bg; (** Reactum *)
  rate : float; (** Rate *)
}
  
(** The type of transition systems. *)
type ctmc = {
  v : Brs.V.t; (** States *)
  e : (int, (int * float)) Hashtbl.t; (** Transition relation *)
  l : (int, int) Hashtbl.t; (** Labelling function *) 
}

(** The type of priority classes. *)
type p_class = 
| P_class of react list  (** Priority class *)
| P_rclass of react list (** Reducable priority class *)

(** Raised when the size of the transition system reaches the limit. *)
exception LIMIT of ctmc

val string_of_react : react -> string

val is_valid_react : react -> bool

val is_valid_p : p_class list -> bool

val is_react_enabled : Big.bg -> react -> bool

(** Compute all the possible evolutions in one step. Total number of occurrences also returned. *)
val step : Big.bg -> react list -> Big.bg list * int

val random_step : Big.bg -> react list -> Big.bg * int

val bfs : Big.bg -> p_class list -> int -> int -> bool -> ctmc * Brs.stats 

val sim : Big.bg -> p_class list -> int -> int -> bool -> ctmc * Brs.stats 
	
val to_dot : ctmc -> string

(**/**)
