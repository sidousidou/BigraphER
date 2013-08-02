(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on stochastic BRS.
@author Michele Sevegnani *)

(** The type of stochastic bigraphical reaction rules.*)
type sreact = {
  rdx : Big.bg; (** Redex *)
  rct : Big.bg; (** Reactum *)
  rate : float; (** Rate *)
}
  
(** The type of Continuous Time Markov Chains. *)
type ctmc = {
  v : Brs.V.t; (** States *)
  e : (int, (int * float)) Hashtbl.t; (** Transition relation *)
  l : (int, int) Hashtbl.t; (** Labelling function *) 
}

(** The type of priority classes: lists of stochastic reaction rules. *)
type p_class = 
| P_class of sreact list  (** Priority class *)
| P_rclass of sreact list (** Reducable priority class *)

(** The type of priority classes: lists of reaction rule identifiers. *)
type p_class_ide = 
| P_class_ide of string list  (** Priority class *)
| P_rclass_ide of string list (** Reducable priority class *)

(** Raised when the size of the transition system reaches the limit. *)
exception LIMIT of ctmc

val string_of_sreact : sreact -> string

val is_valid_sreact : sreact -> bool

val is_valid_p : p_class list -> bool

val is_valid_p_ide : (string -> sreact) -> p_class_ide -> bool

val is_react_enabled : Big.bg -> sreact -> bool

(** Compute all the possible evolutions in one step. Total number of occurrences also returned. *)
val step : Big.bg -> sreact list -> (Big.bg * float) list * int

(*val random_step : Big.bg -> sreact list -> Big.bg * int*)

val bfs : Big.bg -> p_class list -> int -> int -> bool -> ctmc * Brs.stats 

val bfs_ide : Big.bg -> p_class_ide list -> (string -> sreact) -> int -> int -> bool -> ctmc * Brs.stats 

(*val sim : Big.bg -> p_class list -> int -> int -> bool -> ctmc * Brs.stats 

val sim_ide: Big.bg -> p_class_ide list -> (string -> sreact) -> int -> int -> bool -> ctmc * Brs.stats *)

val to_dot : ctmc -> string

(**/**)
