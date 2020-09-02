(** This module defines reactive systems.

    @author Michele Sevegnani *)

(** The type of reactive system kinds. *)
type rs_type =
  | BRS  (** Bigraphical Reactive Systems *)
  | PBRS  (** Probabilistic Bigraphical Reactive Systems *)
  | SBRS  (** Stochasrtic Bigraphical Reactive Systems *)
  | NBRS  (** Nondeterministic Bigraphical Reactive Systems *)

val string_of_rs_type : rs_type -> string
(** String representation of a reactive system kind. *)

type stats = {
  time : float;  (** Build time *)
  states : int;  (** Number of states *)
  trans : int;  (** Number of transitions *)
  occs : int;  (** Number of occurrences *)
}
(** The type of execution statistics for reactive systems. *)

val stats_descr : stats -> (string * string * bool) list
(** Return a description of the execution statistics. It consists of a list
    of triples in the form [(description, value, flag)], where [flag] is
    [true] if the corresponsing property changes across executions, {e e.g.}
    build time. *)

(** Input signature of the functor {!TsType.Make} representing a directed
    graph data structure. *)
module type G = sig
  type t

  type l

  val init : int -> Base.Predicate.t list -> t

  val states : t -> (int * Big.t) Base.H_int.t

  val label : t -> Base.S_predicate.t * int Base.H_predicate.t

  val edges : t -> (int * l * string) Base.H_int.t

  val string_of_l : l -> string
end

(** Input signature of the functor {!Rs.Make} representing the type of
    computation limit. *)
module type L = sig
  type t

  type l

  val init : t

  val increment : t -> l -> t

  val is_greater : t -> t -> bool

  val to_string : t -> string
end

(** Input signature of the functor {!Rs.Make} describing the kind of reactive
    system. *)
module type K = sig
  val typ : rs_type
end

(** Output signature of the functor {!Rs.Make}. *)
module type RS = sig
  type react
  (** The type of bigraphical reaction rules. *)

  type ac
  (** The type of application conditions. *)

  (** The type of priority classes. *)
  type p_class =
    | P_class of react list  (** Priority class. *)
    | P_rclass of react list  (** Reducing priority class. *)

  type graph
  (** The type of transition systems. *)

  type label
  (** The type of edge labels in BRSs. *)

  type limit
  (** Type of simulation limit {e i.e.}, number of execution steps. *)

  val typ : rs_type
  (** The kind of reactive system. *)

  type react_error
  (** The type of reaction validity errors. *)

  val string_of_react : react -> string
  (** String representation of reaction rules. The representation of the
      redex and the reactum are computed by {!val:Big.to_string}. *)

  val name : react -> string
  (** The name of the reaction rule. *)

  val lhs : react -> Big.t
  (** The left-hand side (redex) of a reaction rule. **)

  val rhs : react -> Big.t
  (** The right-hand side (reactum) of a reaction rule. *)

  val label : react -> label
  (** The right-hand side (reactum) of a reaction rule. *)

  val conds : react -> ac list
  (** List of application conditions *)

  val map : react -> Fun.t option
  (** The instantiation map of a reaction rule. *)

  val parse_react_unsafe :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:ac list ->
    label ->
    Fun.t option ->
    react
  (** Create a new reaction rule. If [eta = None], the identity function is
      used as instantiation map. No validity check is performed. *)

  val parse_react :
    name:string ->
    lhs:Big.t ->
    rhs:Big.t ->
    ?conds:ac list ->
    label ->
    Fun.t option ->
    react option
  (** Same as {!val:Brs.parse_react_unsafe} but returns [None] if it is
      impossible to parse a valid reaction. *)

  val string_of_limit : limit -> string
  (** String representation of a simulation limit. *)

  val is_valid_react : react -> bool
  (** Return [true] if the inner (outer) interfaces of the redex (reactum)
      are equal, the redex is solid and the instantiation map is total.
      Return [false] otherwise. See {!val:Big.is_solid}. *)

  exception NOT_VALID of react_error
  (** Raised when a reaction rule is not valid. *)

  val is_valid_react_exn : react -> bool
  (** Same as {!is_valid_react} but an exception is raised when the rule is
      not valid.

      @raise NOT_VALID when the rule is not valid. *)

  val string_of_react_err : react_error -> string
  (** String representation of reaction validity errors. *)

  val equal_react : react -> react -> bool
  (** Equality for reaction rules. *)

  val is_valid_priority : p_class -> bool
  (** Return [true] if a priority class is valid, [false] otherwise. *)

  val is_valid_priority_list : p_class list -> bool
  (** Return [true] if a list of priority classes contains at least a non
      reducible priority class, [false] otherwise.*)

  val cardinal : p_class list -> int
  (** Return the total number of reaction rules in a list of priority
      classes. *)

  val step : Big.t -> react list -> (Big.t * label * react list) list * int
  (** Compute the set of reachable states in one step. Note that isomorphic
      states are merged. The total number of occurrences is also returned. *)

  val random_step :
    Big.t -> react list -> (Big.t * label * react list) option * int
  (** Compute a random state reachable in one step. The total number of
      occurrences is also returned. *)

  val apply : Big.t -> react list -> Big.t option
  (** Sequential application of a list of reaction rules. Non-enabled rules
      are ignored. [None] is returned if no rewriting is performed {e i.e.},
      when all the reaction rules are non-enabled. *)

  val fix : Big.t -> react list -> Big.t * int
  (** Reduce a reducible class to the fixed point. The number of rewriting
      steps is also returned. *)

  val rewrite : Big.t -> p_class list -> Big.t * int
  (** Scan priority classes and reduce a state. Stop when no more rules can
      be applied or when a non reducible priority class is enabled. Also
      return the number of rewriting steps performed in the loop. *)

  exception MAX of graph * stats
  (** Raised when the size of the transition system reaches the maximum
      number of states. *)

  val bfs :
    s0:Big.t ->
    priorities:p_class list ->
    predicates:(Base.Predicate.t * Big.t) list ->
    max:int ->
    (int -> Big.t -> unit) ->
    graph * stats
  (** [bfs ~s0 ~priorities ~max iter_f] computes the transition system of the
      reactive system specified by initial state [s0] and priority classes
      [priorities]. Arguments [~max] and [iter_f] are the maximum number of
      states of the transition system and a function to be applied whenever a
      new state is discovered, respectively. Priority classes are assumed to
      be sorted by priority, {e i.e.}, the first element in the list is the
      class with the highest priority. List of predicates [~predicates] is
      also checked for every state.

      @raise Brs.MAX when the maximum number of states is reached. *)

  exception DEADLOCK of graph * stats * limit
  (** Raised when a simulation reaches a deadlock state. *)

  exception LIMIT of graph * stats
  (** Raised when a simulation reaches the simulation limit. *)

  val sim :
    s0:Big.t ->
    ?seed:int ->
    priorities:p_class list ->
    predicates:(Base.Predicate.t * Big.t) list ->
    init_size:int ->
    stop:limit ->
    (int -> Big.t -> unit) ->
    graph * stats
  (** Simulate the raective system specified by initial state [s0] and
      priority classes [priorities]. Arguments [init_size] and [stop] are the
      initial size of the state set and the simulation limit, respectively.
      Function [iter_f] is applied to every new state discovered during the
      simulation. Optional argument [seed] is used to initialise the random
      generator with a specific seed.

      @raise DEADLOCK when the simulation reaches a deadlock state.
      @raise LIMIT when the simulation limit is exceeded. *)

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
  (** Compute the string representation in [dot] format of a transition
      system. *)

  val to_lab : graph -> string
  (** Compute the string representation in PRISM [lab] format of the
      labelling function of a transition system. *)

  val iter_states : (int -> Big.t -> unit) -> graph -> unit
  (** {2 Iterators} *)

  val fold_states : (int -> Big.t -> 'a -> 'a) -> graph -> 'a -> 'a

  val iter_edges : (int -> int -> label -> unit) -> graph -> unit

  val fold_edges : (int -> int -> label -> 'a -> 'a) -> graph -> 'a -> 'a
end

(** Functor building a concrete implementation of a reactive system. *)
module Make
    (S : Solver.M)
    (R : React.T)
    (P : Priority.P with type r_t := R.t and type r_label := R.label)
    (L : L with type l = R.label)
    (G : G with type l = R.label)
    (K : K) :
  RS
    with type react = R.t
     and type ac := R.ac
     and type label = R.label
     and type graph = G.t
     and type limit = L.t

(**/**)
