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
  }

(** Execution statistics. *)
type stats = {
  time : float; (** Running time in seconds *)
  states : int; (** Number of states in the transition system *)
  reacts : int; (** Number of reactions *)
  occurs : int; (** Number of occurrences found *)
}

(** Module for nodes of a transition system. *)
module V :
sig
  type elt = int * Big.bg
  type t
  val iter : (elt -> unit) -> t -> unit
  val elements : t -> elt list
  val singleton : elt -> t
  val add : elt -> t -> t
  val cardinal : t -> int
  val filter : (elt -> bool) -> t -> t
  val choose : t -> elt
end
  
(** The type of transition systems. *)
type ts = {
  v : V.t; (** States *)
  e : (int, int) Hashtbl.t; (** Transition relation *)
  l : (int, int) Hashtbl.t; (** Labelling function *) 
}

(** The type of priority classes: lists of reaction rules. *)
type p_class = 
| P_class of react list  (** Priority class *)
| P_rclass of react list (** Reducable priority class *)

(** The type of priority classes: lists of reaction rule identifiers. *)
type p_class_ide = 
| P_class_ide of string list  (** Priority class *)
| P_rclass_ide of string list (** Reducable priority class *)

(** Raised when the size of the transition system reaches the limit. *)
exception LIMIT of ts

val string_of_react : react -> string

val is_valid_react : react -> bool

val is_valid_p : p_class -> bool

val is_valid_p_ide : (string -> react) -> p_class_ide -> bool

val is_valid_p_l : p_class list -> bool

val is_valid_p_ide_l : (string -> react) -> p_class_ide list -> bool

val is_react_enabled : Big.bg -> react -> bool

(** Compute all the possible evolutions in one step. Total number of occurrences also returned. *)
val step : Big.bg -> react list -> Big.bg list * int

val random_step : Big.bg -> react list -> Big.bg * int

val bfs : Big.bg -> p_class list -> int -> int -> bool -> ts * stats 

val bfs_ide : Big.bg -> p_class_ide list -> (string -> react) -> int -> int -> bool -> ts * stats 

val sim : Big.bg -> p_class list -> int -> int -> bool -> ts * stats 

val sim_ide: Big.bg -> p_class_ide list -> (string -> react) -> int -> int -> bool -> ts * stats 

val to_dot : ts -> string

val string_of_stats : stats -> string

(**/**)
