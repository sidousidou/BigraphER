(** This module provides operations on BRS.

    @author Michele Sevegnani *)

type react
(** The type of bigraphical reaction rules. *)

(** The type of priority classes, {e i.e.}, lists of reaction rules.
    Intermediate states resulting from the application of reaction rules in
    reducible priority classes are ignored. *)
type p_class =
  | P_class of react list  (** Priority class *)
  | P_rclass of react list  (** Reducible priority class *)

type graph
(** The type of transition systems. *)

type label = unit
(** The type of edge labels in BRSs. *)

type limit = int
(** Type of simulation limit {e i.e.}, number of execution steps. *)

val typ : Rs.t
(** Type of transition system: {{!Rs.t} [BRS]}. *)

val string_of_react : react -> string
(** String representation of reaction rules. The representation of the redex
    and the reactum are computed by {!val:Big.to_string}. *)

val parse_react_unsafe :
  name:string -> lhs:Big.t -> rhs:Big.t -> Fun.t option -> react
(** Create a new reaction rule. If [eta = None], the identity function is
    used as instantiation map. No validity check is performed. *)

val parse_react :
  name:string -> lhs:Big.t -> rhs:Big.t -> Fun.t option -> react option
(** Same as {!val:Brs.parse_react_unsafe} but returns [None] if it is
    impossible to parse a valid reaction. *)

val name : react -> string
(** The name of the reaction rule. *)

val lhs : react -> Big.t
(** The left-hand side (redex) of a reaction rule. **)

val rhs : react -> Big.t
(** The right-hand side (reactum) of a reaction rule. *)

val map : react -> Fun.t option
(** The instantiation map of a reaction rule. *)

val string_of_limit : limit -> string
(** String representation of a simulation limit. *)

val is_valid_react : react -> bool
(** Return [true] if the inner (outer) interfaces of the redex (reactum) are
    equal, the redex is solid and the instantiation map is total. Return
    [false] otherwise. See {!val:Big.is_solid}. *)

val equal_react : react -> react -> bool
(** Equality for reaction rules. *)

type react_error
(** The type of reaction validity errors. *)

exception NOT_VALID of react_error
(** Raised when a reaction rule is not valid. *)

val is_valid_react_exn : react -> bool
(** Same as {!is_valid_react} but an exception is raised when the rule is not
    valid.

    @raise NOT_VALID when the rule is not valid. *)

val string_of_react_err : react_error -> string
(** String representation of reaction validity errors. *)

val is_valid_priority : p_class -> bool
(** Return [true] if all the reaction rules in the priority class are valid,
    [false] otherwise. *)

val is_valid_priority_list : p_class list -> bool
(** Return [true] if a list of priority classes contains at least a non
    reducible priority class, [false] otherwise. *)

val cardinal : p_class list -> int
(** Return the total number of reaction rules in a list of priority classes. *)

val apply : Big.t -> react list -> Big.t option
(** Sequential application of a list of reaction rules. Non-enabled rules are
    ignored. [None] is returned if no rewriting is performed {e i.e.}, when
    all the reaction rules are non-enabled. *)

val step : Big.t -> react list -> (Big.t * label * react list) list * int
(** Compute the set of reachable states in one step. Note that isomorphic
    states are merged. The total number of occurrences is also returned. *)

val random_step :
  Big.t -> react list -> (Big.t * label * react list) option * int
(** Compute a random state reachable in one step. State selection is
    performed according to a uniform distribution over all the possible
    states reachable in one step. The total number of occurrences is also
    returned. *)

val fix : Big.t -> react list -> Big.t * int
(** Reduce a reducible class to the fixed point. The number of rewriting
    steps is also returned. *)

val rewrite : Big.t -> p_class list -> Big.t * int
(** Scan priority classes and reduce a state. Stop when no more rules can be
    applied or when a non reducible priority class is enabled. Also return
    the number of rewriting steps performed in the loop. *)

(** {2 Transition systems} *)

exception MAX of graph * Stats.t
(** Raised when the size of the transition system reaches the maximum number
    of states. *)

val bfs :
  s0:Big.t ->
  priorities:p_class list ->
  predicates:(Base.Predicate.t * Big.t) list ->
  max:int ->
  iter_f:(int -> Big.t -> unit) ->
  graph * Stats.t
(** [bfs ~s0 ~priorities ~max ~iter_f] computes the transition system of the
    BRS specified by initial state [s0] and priority classes [priorities].
    Arguments [~max] and [~iter_f] are the maximum number of states of the
    transition system and a function to be applied whenever a new state is
    discovered, respectively. Priority classes are assumed to be sorted by
    priority, {e i.e.}, the first element in the list is the class with the
    highest priority. List of predicates [~predicates] is also checked for
    every state.

    @raise Brs.MAX when the maximum number of states is reached. *)

(** {2 Simulation traces} *)

exception DEADLOCK of graph * Stats.t * int
(** Raised when the simulation reaches a deadlock state. *)

exception LIMIT of graph * Stats.t
(** Raised when the simulation reaches the maximum number of simulation
    steps. *)

val sim :
  s0:Big.t ->
  priorities:p_class list ->
  predicates:(Base.Predicate.t * Big.t) list ->
  init_size:int ->
  stop:limit ->
  iter_f:(int -> Big.t -> unit) ->
  graph * Stats.t
(** Simulate the BRS specified by initial state [s0] and priority classes
    [priorities]. Arguments [init_size] and [stop] are the initial size of
    the state set and the maximum simulation steps, respectively. Function
    [iter_f] is applied to every new state discovered during the simulation.

    @raise Brs.DEADLOCK when the simulation reaches a deadlock state.
    @raise Brs.LIMIT when the simulation time exceeds the maximum simulation
    steps. *)

(** {2 Export functions} *)

val to_prism : graph -> string
(** Compute the string representation in PRISM [tra] format of a transition
    system. *)

val to_state_rewards : graph -> string
(** Compute the string representation in PRISM [rews] format of state
    rewards. *)

val to_transition_rewards : graph -> string
(** Compute the string representation in PRISM [trew] format of transition
    rewards. *)

val to_dot : graph -> path:string -> name:string -> string
(** Compute the string representation in [dot] format of a transition system. *)

val to_lab : graph -> string
(** Compute the string representation in PRISM [lab] format of the labelling
    function of a transition system. *)

(** {2 Iterators} *)

val iter_states : (int -> Big.t -> unit) -> graph -> unit

val fold_states : (int -> Big.t -> 'a -> 'a) -> graph -> 'a -> 'a

val iter_edges : (int -> int -> label -> unit) -> graph -> unit

val fold_edges : (int -> int -> label -> 'a -> 'a) -> graph -> 'a -> 'a

(**/**)
