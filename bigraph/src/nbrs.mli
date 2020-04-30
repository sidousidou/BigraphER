(** This module provides operations on nondeterministic BRS.

    @author Paulius Dilkas *)

type react
(** The type of nondeterministic bigraphical reaction rules. *)

(** The type of priority classes, {e i.e.}, lists of nondeterministic
    reaction rules. Intermediate states resulting from the application of
    reaction rules in reducible priority classes are ignored. *)
type p_class =
  | P_class of react list  (** Priority class *)
  | P_rclass of react list  (** Reducible priority class *)

type graph
(** The type of Markov Decision Processes. *)

type label = string * int * float
(** The type of edge labels in Markov Decision Processes (MDP), {e i.e.},
    actions, rewards, and weights. *)

type limit = int
(** Type of simulation limit {e i.e.}, number of execution steps. *)

val typ : Rs.t
(** Type of transition system: {{!Rs.t} [NBRS]}. *)

val string_of_react : react -> string
(** Same as {!val:Brs.string_of_react} for nondeterministic reaction rules. *)

val parse_react_unsafe :
  name:string ->
  lhs:Big.t ->
  rhs:Big.t ->
  ?conds:AppCond.t list ->
  label ->
  Fun.t option ->
  react
(** Same as {!val:Brs.parse_react_unsafe} for nondeterministic reaction
    rules. *)

val parse_react :
  name:string ->
  lhs:Big.t ->
  rhs:Big.t ->
  ?conds:AppCond.t list ->
  label ->
  Fun.t option ->
  react option
(** Same as {!val:Brs.parse_react} for nondeterministic reaction rules. *)

val name : react -> string
(** The name of the reaction rule. *)

val action : react -> string
(** The MDP action this reaction rule belongs to. *)

val lhs : react -> Big.t
(** The left-hand side (redex) of a nondeterministic reaction rule. **)

val rhs : react -> Big.t
(** The right-hand side (reactum) of a nondeterministic reaction rule. *)

val conds : react -> AppCond.t list
(** List of application conditions *)

val map : react -> Fun.t option
(** The instantiation map of a reaction rule. *)

val weight : react -> float
(** The weight of a reaction rule. *)

val string_of_limit : limit -> string
(** String representation of a simulation limit. *)

val is_valid_react : react -> bool
(** Same as {!val:Brs.is_valid_react} but also checks that is greater than or
    equal to zero *)

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
(** Return [true] if all the reaction rules in a priority class are valid,
    all the reaction rules in a reducible classes are deterministic. Return
    [false] otherwise. *)

val is_valid_priority_list : p_class list -> bool
(** Return [true] if a list of priority classes contains at least a non
    reducible priority class, [false] otherwise. *)

val cardinal : p_class list -> int
(** Return the total number of nondeterministic reaction rules in a list of
    priority classes. *)

val apply : Big.t -> react list -> Big.t option
(** Same as {!val:Brs.apply}. Note that probabilities are ignored. *)

val step : Big.t -> react list -> (Big.t * label * react list) list * int
(** Compute the set of reachable states in one step. Note that isomorphic
    states are merged and each state is associated to a probability rate. The
    total number of occurrences is also returned. *)

val random_step :
  Big.t -> react list -> (Big.t * label * react list) option * int
(** Compute a random state reachable in one step. The probability of reaching
    a given state depends on the probability associated to the reaction rule
    generating it. The total number of occurrences is also returned. *)

val fix : Big.t -> react list -> Big.t * int
(** Same as {!val:Brs.fix} for nondeterministic reaction rules. Note that
    probabilities are ignored. *)

val rewrite : Big.t -> p_class list -> Big.t * int
(** Scan priority classes and reduce a state. Stop when no more rules can be
    applied or when a non reducible priority class is enabled. Also return
    the number of rewriting steps performed in the loop. *)

(** {2 Markov Decision Processes} *)

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
(** [bfs ~s0 ~priorities ~predicates ~max ~iter_f] computes the transition
    system of the NBRS specified by initial state [s0] and priority classes
    [priorities]. Arguments [~max] and [~iter_f] are the maximum number of
    states of the transition system and a function to be applied whenever a
    new state is discovered, respectively. Priority classes are assumed to be
    sorted by priority, {e i.e}. the first element in the list is the class
    with the highest priority. List of predicates [~predicates] is also
    checked for every state.

    @raise Nbrs.MAX when the maximum number of states is reached. *)

(** {2 Simulation traces} *)

exception DEADLOCK of graph * Stats.t * limit
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
(** Simulate the NBRS specified by initial state [s0] and priority classes
    [priorities]. Arguments [init_size] and [stop] are the initial size of
    the state set and the maximum simulation steps, respectively. Function
    [iter_f] is applied to every new state discovered during the simulation.

    @raise Nbrs.DEADLOCK when the simulation reaches a deadlock state.
    @raise Nbrs.LIMIT when the simulation time exceeds the maximum simulation
    steps. *)

(** {2 Export functions} *)

val to_prism : graph -> string
(** Compute the string representation in PRISM [tra] format of an MDP. *)

val to_state_rewards : graph -> string
(** Compute the string representation in PRISM [rews] format of state
    rewards. *)

val to_transition_rewards : graph -> string
(** Compute the string representation in PRISM [trew] format of transition
    rewards. *)

val to_dot : graph -> path:string -> name:string -> string
(** Compute the string representation in [dot] format of an MDP. *)

val to_lab : graph -> string
(** Compute the string representation in PRISM [lab] format of the labelling
    function of an MDP. *)

(** {2 Iterators} *)

val iter_states : (int -> Big.t -> unit) -> graph -> unit

val fold_states : (int -> Big.t -> 'a -> 'a) -> graph -> 'a -> 'a

val iter_edges : (int -> int -> label -> unit) -> graph -> unit

val fold_edges : (int -> int -> label -> 'a -> 'a) -> graph -> 'a -> 'a

(**/**)
