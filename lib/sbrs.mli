(** This module provides operations on stochastic BRS.
    @author Michele Sevegnani *)

type label = float

(** The type of stochastic bigraphical reaction rules.*)
type react =
  { rdx : Big.bg;                  (** Redex (left-hand side) *)
    rct : Big.bg;                  (** Reactum (right-hand side) *)
    eta : Fun.t option;            (** Instantiation map: a total function from the sites on the rhs to the sites on the lhs  *)
    rate : label                   (** Stochastic rate (always > 0) *)
  }

(** The type of priority classes, {e i.e.} lists of stochastic reaction
    rules. Intermediate states resulting from the application of reaction rules
    in reducible priority classes are ignored. *)
type p_class = 
  | P_class of react list  (** Priority class *)
  | P_rclass of react list (** Reducible priority class *)

(** Execution statistics. *)		      
type stats = TsType.stats_t

(** The type of Continuous Time Markov Chains (CTMC). *)
type graph = {
  v : (int * Big.bg) Base.H_int.t;      (** States *)
  e : (int * float) Base.H_int.t;       (** Transition relation *)
  l : int Base.H_string.t;              (** Labelling function *) 
  preds : Base.S_string.t;              (** Predicate identifiers *)
}

(** Type of occurrences *)
type occ = Big.bg * float

(** Type of simulation limit *)
type limit = float

(** Type of transition system: {{!Rs.t}[SBRS]} . *)
val typ : Rs.t

(** Stats are represented as a list whose elements are strings in the
    following form: [(description, value, flag)]. [flag] is [true] iff it is
    attached to a value that depends on the current run. *)
val string_of_stats : stats -> (string * string * bool) list

(** String representation of a stochastic reaction rule. *)
val string_of_react : react -> string

(** Create a new reaction rule. *)
val parse_react : lhs:Big.bg -> rhs:Big.bg -> float option -> Fun.t option -> react

(** The left-hand side (redex) of a reaction rule. **)
val lhs_of_react : react -> Big.bg

(** The right-hand side (reactum) of a reaction rule. *)
val rhs_of_react : react -> Big.bg
 
(** String representation of a simulation limit. *)
val string_of_limit : limit -> string
  
(** Return [true] if the inner (outer) interfaces of the redex (reactum) are
    equal, the redex is solid, the instantiation map is total and the rate is
    greater than zero. Return [false] otherwise. *)
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

(** Return [true] if all the reaction rules in a priority class are
    valid, all the reaction rules in a reducible classes are
    instantaneous and no instantaneous reaction rules are present in
    a non reducible class. Return [false] otherwise. *)
val is_valid_priority : p_class -> bool

(** Return [true] if a list of priority classes contains at least a non
    reducible priority class, [false] otherwise. *)
val is_valid_priority_list : p_class list -> bool

(** Return the total number of stochastic reaction rules in a list of priority
    classes. *)
val cardinal : p_class list -> int

(** Sequential application of a list of reaction rules. Non-enabled rules are
    ignored. *)
val apply : Big.bg -> react list -> Big.bg option

(** Compute the set of reachable states in one step. Note that isomorphic states
    are merged and each state is associated to a transition rate. The total
    number of occurrences is also returned. *)
val step : Big.bg -> react list -> (Big.bg * float) list * int

(** Select step of {{: https://en.wikipedia.org/wiki/Gillespie_algorithm}
    Gillespie SSA}. The total number of occurrences is also returned. *)
val random_step : Big.bg -> react list -> (Big.bg * float) option * int 

(** Reduce a reducible class to the fixed point. The number of rewriting steps
    is also returned. *)
val fix : Big.bg -> react list -> Big.bg * int

(** Scan priority classes and reduce a state. Stop when no more rules can be
    applied or when a non reducible priority class is enabled. Also return the
    number of rewriting steps performed in the loop. *)
val rewrite : Big.bg -> p_class list -> Big.bg * int
  
(** {3 Continuous Time Markov Chains} *)

(** Raised when the size of the transition system reaches the maximum number of
    states. *)
exception MAX of graph * stats

(** [bfs ~s0 ~priorities ~predicates ~max ~iter_f] computes the transition
    system of the SBRS specified by initial state [s0] and priority classes
    [priorities]. Arguments [~max] and [~iter_f] are the maximum number of
    states of the transition system and a function to be applied whenever a new
    state is discovered, respectively. Priority classes are assumed to be sorted
    by priority, {e i.e}. the first element in the list is the class with the
    highest priority. List of predicates [~predicates] is also checked for every
    state.

    @raise Sbrs.MAX when the maximum number of states is reached. *)
val bfs : s0:Big.bg ->
  priorities:p_class list ->
  predicates:(string * Big.bg) list ->
  max:int ->
  iter_f:(int -> Big.bg -> unit) ->
  graph * stats

(** {3 Stochastic simulation traces} *)

(** Raised when the simulation reaches a deadlock state. *)			     
exception DEADLOCK of graph * stats * float

(** Raised when the simulation reaches the maximum simulation time. *)
exception LIMIT of graph * stats

(** Simulate (using Gillespie SSA) the SBRS specified by initial state [s0] and
    priority classes [priorities]. Arguments [init_size] and [stop] are the
    initial size of the state set and the maximum simulation time,
    respectively. Function [iter_f] is applied to every new state discovered
    during the simulation.

    @raise Sbrs.DEADLOCK when the simulation reaches a deadlock state.
    @raise Sbrs.LIMIT when the simulation time exceeds the maximum simulation time. *)
val sim :
  s0:Big.bg ->
  priorities:p_class list ->
  predicates:(string * Big.bg) list ->
  init_size:int ->
  stop:limit ->
  iter_f:(int -> Big.bg -> unit) ->
  graph * stats

(** {3 Export functions} *)

(** Compute the string representation in PRISM [tra] format of a CTMC. *)
val to_prism : graph -> string

(** Compute the string representation in [dot] format of a CTMC. *)
val to_dot : graph -> name:string -> string

(** Compute the string representation in PRISM [lab] format of the labelling
    function of a CTMC. *)
val to_lab : graph -> string

(** Apply [f] to every state. *)			
val iter_states : f:(int -> Big.bg -> unit) -> graph -> unit

(** Export to file the string representation in [dot] format of a CTMC. 

    @raise Rs.EXPORT_ERROR when an error occurs. *)
val write_dot : graph -> name:string -> path:string -> int

(** Export to file the string representation in PRISM [lab] format of the
    labelling function of a CTMC. 

    @raise Rs.EXPORT_ERROR when an error occurs. *)
val write_lab : graph -> name:string -> path:string -> int

(** Export to file the string representation in PRISM [tra] format of a
    CTMC. 

    @raise Rs.EXPORT_ERROR when an error occurs. *)
val write_prism : graph -> name:string -> path:string -> int

(** Export to file the string representation in [svg] format of a CTMC. 

    @raise Rs.EXPORT_ERROR when an error occurs. *)
val write_svg : graph -> name:string -> path:string -> int

(** Export to file the string representation in [json] format of a CTMC. 

    @raise Rs.EXPORT_ERROR when an error occurs. *)
val write_json : graph -> name:string -> path:string -> int

(**/**)
