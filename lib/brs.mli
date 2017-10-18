(** This module provides operations on BRS.
    @author Michele Sevegnani *)

(** The type of bigraphical reaction rules. *)
type react =
  { rdx : Big.bg;                  (** Redex (left-hand side) *)
    rct : Big.bg;                  (** Reactum (right-hand side) *)
    eta : int Fun.t option         (** Instantiation map: a total function from the sites on the rhs to the sites on the lhs  *)
  }

(** The type of priority classes, {e i.e.} lists of reaction rules. Intermediate
    states resulting from the application of reaction rules in reducible
    priority classes are ignored. *)
type p_class =
  | P_class of react list  (** Priority class *)
  | P_rclass of react list (** Reducible priority class *)

(** Execution statistics. *)		      
type stats  = TsType.stats_t

(** The type of transition systems. *)
type graph = {
  v : (int * Big.bg) Base.H_int.t;  (** States *)
  e : int Base.H_int.t;             (** Transition relation *)
  l : int Base.H_string.t;          (** Labelling function *)
  preds : Base.S_string.t;          (** Predicate identifiers *)
}

(** Type of occurrences *)
type occ = Big.bg

(** Type of simulation limit *)
type limit = int

type label = float

(** Type of transition system: {{!Rs.t}[BRS]}. *)
val typ : Rs.t

(** Stats are represented as a list whose elements are strings in the
    following form: [(description, value, flag)]. [flag] is [true] iff it is
    attached to a value that depends on the current run. *)
val string_of_stats : stats -> (string * string * bool) list

(** String representation of reaction rules. *)
val string_of_react : react -> string

(** Create a new reaction rule. *)
val parse_react : lhs:Big.bg -> rhs:Big.bg -> float option -> int Fun.t option -> react

(** The left-hand side (redex) of a reaction rule. **)
val lhs_of_react : react -> Big.bg

(** The right-hand side (reactum) of a reaction rule. *)
val rhs_of_react : react -> Big.bg

(** String representation of a simulation limit. *)
val string_of_limit : limit -> string

(** Return [true] if the inner (outer) interfaces of the redex (reactum) are
    equal, the redex is solid and the instantiation map is total. Return [false]
    otherwise. *)
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

(** Return [true] if all the reaction rules in the priority class are valid,
    [false] otherwise. *)
val is_valid_priority : p_class -> bool

(** Return [true] if a list of priority classes contains at least a non reducible
    priority class, [false] otherwise. *)
val is_valid_priority_list : p_class list -> bool

(** Return the total number of reaction rules in a list of priority classes. *)
val cardinal : p_class list -> int

(** Sequential application of a list of reaction rules. Non-enabled rules are
    ignored. The input bigraph is returned unchanged if no reaction rules can be
    applied. *)
val apply : Big.bg -> react list -> Big.bg

(** Compute the set of reachable states in one step. Note that isomorphic states
    are merged. The total number of occurrences is also returned. *)
val step : Big.bg -> react list -> Big.bg list * int

(** Compute a random state reachable in one step. State selection is performed
    according to a uniform distribution over all the possible states reachable
    in one step. The total number of occurrences is also returned. *)
val random_step : Big.bg -> react list -> Big.bg option * int

(** Reduce a reducible class to the fixed point. The number of rewriting steps
    is also returned. *)
val fix : Big.bg -> react list -> Big.bg * int

(** Scan priority classes and reduce a state. Stop when no more rules can be
    applied or when a non reducible priority class is enabled. Also return the
    number of rewriting steps performed in the loop. *)
val rewrite : Big.bg -> p_class list -> Big.bg * int

(** {3 Transition systems} *)

(** Raised when the size of the transition system reaches the maximum number of
    states. *)
exception MAX of graph * stats

(** [bfs ~s0 ~priorities ~max ~iter_f] computes the transition system of the
    BRS specified by initial state [s0] and priority classes
    [priorities]. Arguments [~max] and [~iter_f] are the maximum number of
    states of the transition system and a function to be applied whenever a new
    state is discovered, respectively. Priority classes are assumed to be
    sorted by priority, {e i.e}. the first element in the list is the class with
    the highest priority. List of predicates [~predicates] is also checked for every
    state.

    @raise Brs.MAX when the maximum number of states is reached. *)
val bfs : s0:Big.bg ->
  priorities:p_class list ->
  predicates:(string * Big.bg) list ->
  max:int ->
  iter_f:(int -> Big.bg -> unit) ->
  graph * stats

(** {3 Simulation traces} *)

(** Raised when the simulation reaches a deadlock state. *)
exception DEADLOCK of graph * stats * int

(** Raised when the simulation reaches the maximum number of simulation steps. *)
exception LIMIT of graph * stats					

(** Simulate the BRS specified by initial state [s0] and
    priority classes [priorities]. Arguments [init_size] and [stop] are the
    initial size of the state set and the maximum simulation steps,
    respectively. Function [iter_f] is applied to every new state discovered
    during the simulation.

    @raise Brs.DEADLOCK when the simulation reaches a deadlock state.
    @raise Brs.LIMIT when the simulation time exceeds the maximum simulation steps. *)
val sim : s0:Big.bg ->
  priorities:p_class list ->
  predicates:(string * Big.bg) list ->
  init_size:int ->
  stop:limit ->
  iter_f:(int -> Big.bg -> unit) ->
  graph * stats

(** {3 Export functions} *)

(** Compute the string representation in PRISM [tra] format of a transition
    system. *)							    
val to_prism : graph -> string

(** Compute the string representation in [dot] format of a transition system. *)
val to_dot : graph -> name:string -> string

(** Compute the string representation in PRISM [lab] format of the labelling
    function of a transition system. *)				       
val to_lab : graph -> string

(** Apply [f] to every state. *)
val iter_states : f:(int -> Big.bg -> unit) -> graph -> unit

(** Export to file the string representation in [dot] format of a transition
    system.

    @raise Rs.EXPORT_ERROR when an error occurs. *)
val write_dot : graph -> name:string -> path:string -> int

(** Export to file the string representation in PRISM [lab] format of the
    labelling function of a transition system.

    @raise Rs.EXPORT_ERROR when an error occurs. *)
val write_lab : graph -> name:string -> path:string -> int

(** Export to file the string representation in PRISM [tra] format of a
    transition system.

    @raise Rs.EXPORT_ERROR when an error occurs. *)
val write_prism : graph -> name:string -> path:string -> int

(** Export to file the string representation in [svg] format of a transition
    system.

    @raise Rs.EXPORT_ERROR when an error occurs. *)
val write_svg : graph -> name:string -> path:string -> int

(** Export to file the string representation in [json] format of a transition
    system.

    @raise Rs.EXPORT_ERROR when an error occurs. *)
val write_json : graph -> name:string -> path:string -> int

(**/**)
