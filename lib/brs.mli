(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides operations on BRS.
    @author Michele Sevegnani
    @version 0.3.0 *)

(** The type of bigraphical reaction rules.*)
type react = {
    rdx : Big.bg; (** Redex *)
    rct : Big.bg; (** Reactum *)
  }
  
(** The type of transition systems. *)
type ts = {
  v : (Big.bg_key, (int * Big.bg)) Hashtbl.t; (** States *)
  e : (int, int) Hashtbl.t; (** Transition relation *)
  l : (int, int) Hashtbl.t; (** Labelling function *) 
}

(** The type of priority classes: lists of reaction rules. *)
type p_class = 
| P_class of react list  (** Priority class *)
| P_rclass of react list (** Reducible priority class *)

(** The type of priority classes: lists of reaction rule identifiers. *)
type p_class_ide = 
| P_class_ide of string list  (** Priority class *)
| P_rclass_ide of string list (** Educable priority class *)

(** Raised when the size of the transition system reaches the limit. *)
exception LIMIT of ts

(** String representation of a reaction. *)
val string_of_react : react -> string

(** Return [true] if the inner (outer) interfaces of the redex (reactum) are
    equal and if the redex is solid. [false] otherwise. *)
val is_valid_react : react -> bool

(** Return [true] if all the reaction rules in the priority class are valid,
    [false] otherwise. *)
val is_valid_p : p_class -> bool

(** Same as {!Brs.is_valid_p} but the priority class is of type {!Brs.p_class}. *)
val is_valid_p_ide : (string -> react) -> p_class_ide -> bool

(** Return [true] if a list of priority classes contains at least a non reducing
    priority class, [false] otherwise. *)
val is_valid_p_l : p_class list -> bool

(** Same as {!Brs.is_valid_p_l} but priority classes are of type
    {!Brs.p_class}. *)
val is_valid_p_ide_l : p_class_ide list -> bool

(** Return [true] if a reaction can be applied on a bigraph. *)
val is_react_enabled : Big.bg -> react -> bool

(** Return [true] if there is a reaction rule within the input priority class
    that can be applied. *)
val is_class_enabled : Big.bg -> react list -> bool

(** Compute all the possible evolutions in one step. Total number of occurrences
    also returned. *)
val step : Big.bg -> react list -> Big.bg list * int

(** Compute a random reaction.
    @raise Big.NO_MATCH when no reaction can be computed.*)
val random_step : Big.bg -> react list -> Big.bg * int

(** Reduce a reducible class to the fixed point. Return the input state if no
    rewriting is performed. The fixed point and the number of rewriting steps
    performed are returned otherwise. *)   
val fix : Big.bg -> react list -> Big.bg * int

(** Scan priority classes and reduce a state. Stop when no more rules can be
    applied or when a non reducing priority class is enabled. The output integer
    is the number of rewriting steps performed in the loop. *)
val rewrite : Big.bg -> p_class list -> int -> Big.bg * int

(** Same as {Brs.rewrite} but priority classes are of type {Brs.p_class_ide}. *)
val rewrite_ide : (string -> react) -> Big.bg -> p_class_ide list -> int -> Big.bg * int

(** [bfs s p l n v] computes the transition system of the BRS specified by
    initial state [s] and priority classes [p]. [l] is the maximum number of
    states of the transition system. [n] is the initialisation size for the
    edges and [v] is a verbosity flag. Priority classes are assumed to
    be sorted by priority, i.e. the first element in the list is the class with
    the highest priority.
    @raise Brs.LIMIT when the maximum number of states is reached.*)
val bfs : Big.bg -> p_class list -> int -> int -> bool -> ts * (float * int * int * int) 

(** Same as {!Brs.bfs} but priority classes are of type {!Brs.p_class_ide}.*)
val bfs_ide : Big.bg -> p_class_ide list -> (string -> react) -> int -> int -> bool -> ts * (float * int * int * int)

(** Similar to {!Brs.bfs} but only one simulation path is computed. In this
    case, parameter [l] indicates the maximum number of simulation steps. *)
val sim : Big.bg -> p_class list -> int -> int -> bool -> ts * (float * int * int * int)

(** Same as {!Brs.sim} but priority classes are of type {!Brs.p_class_ide}. *)
val sim_ide: Big.bg -> p_class_ide list -> (string -> react) -> int -> int -> bool -> ts * (float * int * int * int)

(** String representation of BRS execution statistics. *)
val string_of_stats : float * int* int * int -> string

(** Textual representation of a transition system. The format is compatible
    with PRISM input format. *)
val to_prism : ts -> string

(** Compute the string representation in [dot] format of a transition system. *)
val to_dot : ts -> string

val iter_states : (int -> Big.bg -> unit) -> ts -> unit

(**/**)
