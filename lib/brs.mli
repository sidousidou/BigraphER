(** This module provides operations on BRS.
    @author Michele Sevegnani *)

(** The type of bigraphical reaction rules.*)
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
type stats =  { time : float;  (** Execution time *)
		states : int;  (** Number of states *)
		trans : int;   (** Number of transitions *)
		occs : int;    (** Number of occurrences *)
	      }

(** The type of transition systems. *)
type graph = {
    v : (Big.bg_key, (int * Big.bg)) Hashtbl.t; (** States *)
    e : (int, int) Hashtbl.t;                   (** Transition relation *)
    l : (int, int) Hashtbl.t;                   (** Labelling function *) 
  }
	       
(** String representation of a reaction reaction. *)
val to_string_react : react -> string

(** Return [true] if the inner (outer) interfaces of the redex (reactum) are
    equal, the redex is solid and the instantiation map is total. Return [false]
    otherwise. *)
val is_valid_react : react -> bool

(** Return [true] if all the reaction rules in the priority class are valid,
    [false] otherwise. *)
val is_valid_priority : p_class -> bool

(** Return [true] if a list of priority classes contains at least a non reducible
    priority class, [false] otherwise. *)
val is_valid_priority_list : p_class list -> bool

(** Return the total number of reaction rules in a list of priority classes. *)
val cardinal : p_class list -> int
					       
(** Compute the set of reachable states in one step. Note that isomorphic states
    are merged. The total number of occurrences is also returned. *)
val step : Big.bg -> react list -> Big.bg list * int

(** Compute a random state reachable in one step. The total number of
    occurrences is also returned. *)
val random_step : Big.bg -> react list -> Big.bg option * int
						   
(** Reduce a reducible class to the fixed point. The number of rewriting steps
    is also returned. *)
val fix : Big.bg -> react list -> Big.bg * int

(** Scan priority classes and reduce a state. Stop when no more rules can be
    applied or when a non reducible priority class is enabled. Also return the
    number of rewriting steps performed in the loop. *)
val rewrite : Big.bg -> p_class list -> Big.bg * int
						   
(** {6 Transition systems} *)
						   
(** Raised when the size of the transition system reaches the maximum number of
    states. *)
exception MAX of graph * stats
			   
(** [bfs ~s0 ~priorities ~max ~iter_f] computes the transition system of the
    BRS specified by initial state [s0] and priority classes
    [priorities]. Arguments [~max] and [~iter_f] are the maximum number of
    states of the transition system and a function to be applied whenever a new
    state is discovered, respectively. Priority classes are assumed to be
    sorted by priority, {e i.e}. the first element in the list is the class with
    the highest priority.

    @raise Brs.MAX when the maximum number of states is reached. *)
val bfs : s0:Big.bg -> priorities:p_class list -> max:int ->
	  iter_f:(int -> Big.bg -> unit) -> graph * stats

(** {6 Simulation traces} *)
						      
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
val sim :
  s0:Big.bg ->
  priorities:p_class list -> init_size:int ->
  stop:int -> iter_f:(int -> Big.bg -> unit) -> graph * stats

(** {6 Export functions} *)

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
    system. *)							  
val write_dot : graph -> name:string -> path:string -> int

(** Export to file the string representation in PRISM [lab] format of the
    labelling function of a transition system. *)
val write_lab : graph -> name:string -> path:string -> int

(** Export to file the string representation in PRISM [tra] format of a
    transition system. *)							 
val write_prism : graph -> name:string -> path:string -> int

(** Export to file the string representation in [svg] format of a transition
    system. *)							 
val write_svg : graph -> name:string -> path:string -> int

(**/**)
