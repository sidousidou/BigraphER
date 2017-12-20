(** This module provides operations on stochastic BRS.
    @author Michele Sevegnani *)

(** The type of stochastic bigraphical reaction rules.*)
type react

(** The type of priority classes, {e i.e.}, lists of stochastic reaction
    rules. Intermediate states resulting from the application of reaction rules
    in reducible priority classes are ignored. *)
type p_class =
  | P_class of react list  (** Priority class *)
  | P_rclass of react list (** Reducible priority class *)

(** The type of Continuous Time Markov Chains (CTMC). *)
type graph

(** Type of occurrences {e i.e.}, a bigraph and a *)
type occ = Big.t * float

(** Type of simulation limit {e i.e.}, execution time. *)
type limit = float

(** Type of transition system: {{!Rs.t}[SBRS]}. *)
val typ : Rs.t

(** Same as {!val:Brs.string_of_react} for stochastic reaction rules. *)
val string_of_react : react -> string

(** Same as {!val:Brs.parse_react_unsafe} for stochastic reaction rules. *)
val parse_react_unsafe : lhs:Big.t -> rhs:Big.t -> float -> Fun.t option -> react

(** Same as {!val:Brs.parse_react} for stochastic reaction rules. *)
val parse_react : lhs:Big.t -> rhs:Big.t -> float -> Fun.t option -> react option

(** The left-hand side (redex) of a stochastic reaction rule. **)
val lhs : react -> Big.t

(** The right-hand side (reactum) of a stochastic reaction rule. *)
val rhs : react -> Big.t

(** The instantiation map of a reaction rule. *)
val map : react -> Fun.t option

(** The rate of a reaction rule. *)
val rate : react -> float

(** String representation of a simulation limit. *)
val string_of_limit : limit -> string

(** Same as {!val:Brs.is_valid_react} but also checks that the rate is greater
    than zero. Return [false] otherwise. See {!val:Big.is_solid}. *)
val is_valid_react : react -> bool

(** The type of reaction validity errors. *)
type react_error

(** Raised when a reaction rule is not valid. *)
exception NOT_VALID of react_error

(** Same as {!is_valid_react} but an exception is raised when the rule is not
    valid.

    @raise NOT_VALID when the rule is not valid. *)
val is_valid_react_exn : react -> bool

(** String representation of reaction validity errors. *)
val string_of_react_err : react_error -> string

(** Return [true] if a reaction rule is instantaneous, [false] otherwise. *)
val is_inst : react -> bool

(** Return [true] if all the reaction rules in a priority class are valid, all
    the reaction rules in a reducible classes are instantaneous and no
    instantaneous reaction rules are present in a non reducible class. Return
    [false] otherwise. *)
val is_valid_priority : p_class -> bool

(** Return [true] if a list of priority classes contains at least a non
    reducible priority class, [false] otherwise. *)
val is_valid_priority_list : p_class list -> bool

(** Return the total number of stochastic reaction rules in a list of priority
    classes. *)
val cardinal : p_class list -> int

(** Same as {!val:Brs.apply}. Note that stochastic rates are ignored. *)
val apply : Big.t -> react list -> Big.t option

(** Compute the set of reachable states in one step. Note that isomorphic states
    are merged and each state is associated to a transition rate. The total
    number of occurrences is also returned. *)
val step : Big.t -> react list -> occ list * int

(** Select step of {{: https://en.wikipedia.org/wiki/Gillespie_algorithm}
    Gillespie SSA}. The total number of occurrences is also returned. *)
val random_step : Big.t -> react list -> occ option * int

(** Same as {!val:Brs.fix} for probabilistic reaction rules. Note that
    stochastic rates are ignored. *)
val fix : Big.t -> react list -> Big.t * int

(** Scan priority classes and reduce a state. Stop when no more rules can be
    applied or when a non reducible priority class is enabled. Also return the
    number of rewriting steps performed in the loop. *)
val rewrite : Big.t -> p_class list -> Big.t * int

(** {3 Continuous Time Markov Chains} *)

(** Raised when the size of the transition system reaches the maximum number of
    states. *)
exception MAX of graph * Stats.t

(** [bfs ~s0 ~priorities ~predicates ~max ~iter_f] computes the transition
    system of the SBRS specified by initial state [s0] and priority classes
    [priorities]. Arguments [~max] and [~iter_f] are the maximum number of
    states of the transition system and a function to be applied whenever a new
    state is discovered, respectively. Priority classes are assumed to be sorted
    by priority, {e i.e}. the first element in the list is the class with the
    highest priority. List of predicates [~predicates] is also checked for every
    state.

    @raise Sbrs.MAX when the maximum number of states is reached. *)
val bfs : s0:Big.t ->
  priorities:p_class list ->
  predicates:(string * Big.t) list ->
  max:int ->
  iter_f:(int -> Big.t -> unit) ->
  graph * Stats.t

(** {3 Stochastic simulation traces} *)

(** Raised when the simulation reaches a deadlock state. *)
exception DEADLOCK of graph * Stats.t * float

(** Raised when the simulation reaches the maximum simulation time. *)
exception LIMIT of graph * Stats.t

(** Simulate (using Gillespie SSA) the SBRS specified by initial state [s0] and
    priority classes [priorities]. Arguments [init_size] and [stop] are the
    initial size of the state set and the maximum simulation time,
    respectively. Function [iter_f] is applied to every new state discovered
    during the simulation.

    @raise Sbrs.DEADLOCK when the simulation reaches a deadlock state.
    @raise Sbrs.LIMIT when the simulation time exceeds the maximum simulation time. *)
val sim :
  s0:Big.t ->
  priorities:p_class list ->
  predicates:(string * Big.t) list ->
  init_size:int ->
  stop:limit ->
  iter_f:(int -> Big.t -> unit) ->
  graph * Stats.t

(** {3 Export functions} *)

(** Compute the string representation in PRISM [tra] format of a CTMC. *)
val to_prism : graph -> string

(** Compute the string representation in [dot] format of a CTMC. *)
val to_dot : graph -> name:string -> string

(** Compute the string representation in PRISM [lab] format of the labelling
    function of a CTMC. *)
val to_lab : graph -> string

(** Apply [f] to every state. *)
val iter_states : f:(int -> Big.t -> unit) -> graph -> unit

(**/**)
