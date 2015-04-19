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
  v : (Big.bg_key, (int * Big.bg)) Hashtbl.t; (** States *)
  e : (int, (int * float)) Hashtbl.t; (** Transition relation *)
  l : (int, int) Hashtbl.t; (** Labelling function *) 
}

type stats = {
  t : float;    (** Execution time *)
  sim : float;  (** Simulation time *)
  s : int;      (** Number of states *)
  r : int;      (** Number of reaction *)
  o : int;      (** Number of occurrences *)
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
exception LIMIT of ctmc * stats

(** String representation of a reaction. *)
val string_of_sreact : sreact -> string

(** Return [true] if the inner (outer) interfaces of the redex (reactum) are
    equal, if the redex is solid and if the rate is greater than zero.
    Return [false] otherwise. *)
val is_valid_sreact : sreact -> bool

(** Return [true] if a reaction rule is instantaneous, [false] otherwise. *)
val is_inst : sreact -> bool

(** Return [true] if all the reaction rules in a priority class are
    valid, all the reaction rules in a reducible classes are
    instantaneous and no instantaneous raction rules are present in
    a non reducible class. Return [false] otherwise. *)
val is_valid_p : p_class -> bool

(** Same as {!Sbrs.is_valid_p} but the priority class is of type
    {!Sbrs.p_class}. *)
val is_valid_p_ide : (string -> sreact) -> p_class_ide -> bool

(** Return [true] if a list of priority classes contains at least a non reducing
    priority class, [false] otherwise. *)
val is_valid_p_l : p_class list -> bool

(** Same as {!Sbrs.is_valid_p_l} but priority classes are of type
    {!Sbrs.p_class}. *)
val is_valid_p_ide_l : p_class_ide list -> bool

(** Return [true] if a reaction can be applied on a bigraph. *)
val is_sreact_enabled : Big.bg -> sreact -> bool

(** Return [true] if there is a reaction rule within the input priority class
    that can be applied. *)
val is_class_enabled : Big.bg -> sreact list -> bool

(** Compute all the possible evolutions in one step. Total number of occurrences
    is also returned. *)
val step : Big.bg -> sreact list -> (Big.bg * float) list * int

(** Select step of Gillespie's SSA. Total number of occurrence is also returned. 
    @raise Big.NO_MATCH when no reaction can be computed.*)
val select_sreact : Big.bg -> sreact list -> int -> (Big.bg * float) * int 

(** Reduce a reducible class to the fixed point. Return the input state if no
    rewriting is performed. The fixed point and the number of rewriting steps
    performed are returned otherwise. *)   
val fix : Big.bg -> sreact list -> Big.bg * int

(** Scan priority classes and reduce a state. Stop when no more rules can be
    applied or when a non reducing priority class is enabled. The output integer
    is the number of rewriting steps performed in the loop. *)
val rewrite : Big.bg -> p_class list -> int -> Big.bg * int

(** Same as {Sbrs.rewrite} but priority classes are of type {Sbrs.p_class_ide}. *)
val rewrite_ide : (string -> sreact) -> Big.bg -> p_class_ide list -> int -> Big.bg * int

(** [bfs s p l n f] computes the transition system of the SBRS specified by
    initial state [s] and priority classes [p]. [l] is the maximum number of
    states of the transition system. [n] is the initialisation size for the
    edges and [f] is function to be applied whenever a new state is discovered. 
    Priority classes are assumed to be sorted by priority, i.e. the first element 
    in the list is the class with the highest priority.
    @raise Sbrs.LIMIT when the maximum number of states is reached.*)
val bfs : Big.bg -> p_class list -> int -> int -> 
  (int -> Big.bg -> unit) -> ctmc * stats

(** Same as {!Sbrs.bfs} but priority classes are of type {!Sbrs.p_class_ide}.*)
val bfs_ide : Big.bg -> p_class_ide list -> (string -> sreact) -> int -> int -> 
  (int -> Big.bg -> unit) -> ctmc * stats 

(** Similar to {!Sbrs.bfs} but only one simulation path is computed. 
    @raise Sbrs.LIMIT when the simulation reaches the time limit. *)
val sim : Big.bg -> p_class list -> float -> int -> 
  (int -> Big.bg -> unit) -> ctmc * stats 

(** Same as {!Sbrs.sim} but priority classes are of type {!Sbrs.p_class_ide}.
    @raise Sbrs.LIMIT_SIM when the simulation reaches the time limit. *)
val sim_ide: Big.bg -> p_class_ide list -> (string -> sreact) -> float -> int ->
    (int -> Big.bg -> unit) -> ctmc * stats

(** String representation of SBRS execution statistics. *)
val string_of_stats : stats -> string

(** Textual representation of a ctmc. The format is compatible with PRISM input 
    format. *)
val to_prism : ctmc -> string

(** Compute the string representation in [dot] format of a transition system. *)
val to_dot : ctmc -> string

val iter_states : (int -> Big.bg -> unit) -> ctmc -> unit

(**/**)
