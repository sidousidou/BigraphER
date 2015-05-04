(** This module provides operations on stochastic BRS.
    @author Michele Sevegnani *)

(** The type of stochastic bigraphical reaction rules.*)
type sreact =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option;        (* Instantiation map *)
    rate : float
  }

(** The type of priority classes: lists of stochastic reaction rules. *)
type p_class = 
  | P_class of sreact list  (** Priority class *)
  | P_rclass of sreact list (** Reducable priority class *)

(** Execution statistics. *)		      
type stats =  { time : float; 
		states : int;  
		trans : int;  
		occs : int;
	      }
		       
(** The type of Continuous Time Markov Chains. *)
type graph = {
  v : (Big.bg_key, (int * Big.bg)) Hashtbl.t; (** States *)
  e : (int, (int * float)) Hashtbl.t; (** Transition relation *)
  l : (int, int) Hashtbl.t; (** Labelling function *) 
}

(** String representation of a reaction. *)
val to_string_react : sreact -> string

(** Return [true] if the inner (outer) interfaces of the redex (reactum) are
    equal, if the redex is solid and if the rate is greater than zero.
    Return [false] otherwise. *)
val is_valid_react : sreact -> bool

(** Return [true] if a reaction rule is instantaneous, [false] otherwise. *)
val is_inst : sreact -> bool

(** Return [true] if all the reaction rules in a priority class are
    valid, all the reaction rules in a reducible classes are
    instantaneous and no instantaneous raction rules are present in
    a non reducible class. Return [false] otherwise. *)
val is_valid_priority : p_class -> bool

(** Return [true] if a list of priority classes contains at least a non reducing
    priority class, [false] otherwise. *)
val is_valid_priority_list : p_class list -> bool

(** Compute all the possible evolutions in one step. Total number of occurrences
    is also returned. *)
val step : Big.bg -> sreact list -> (Big.bg * float) list * int

(** Select step of Gillespie's SSA. Total number of occurrence is also returned. 
    @raise Big.NO_MATCH when no reaction can be computed.*)
val random_step : Big.bg -> sreact list -> (Big.bg * float) option * int 

(** Reduce a reducible class to the fixed point. Return the input state if no
    rewriting is performed. The fixed point and the number of rewriting steps
    performed are returned otherwise. *)   
val fix : Big.bg -> sreact list -> Big.bg * int

(** Scan priority classes and reduce a state. Stop when no more rules can be
    applied or when a non reducing priority class is enabled. The output integer
    is the number of rewriting steps performed in the loop. *)
val rewrite : Big.bg -> p_class list -> Big.bg * int

(** {6 Exhaustive exploration of the state space} *)

module Ctmc : sig

    (** Raised when the size of the transition system reaches the limit. *)
    exception MAX of graph * stats

    (** [bfs s p l n f] computes the transition system of the SBRS specified by
        initial state [s] and priority classes [p]. [l] is the maximum number of
        states of the transition system. [n] is the initialisation size for the
        edges and [f] is function to be applied whenever a new state is
        discovered.  Priority classes are assumed to be sorted by priority,
        i.e. the first element in the list is the class with the highest
        priority.  

        @raise Sbrs.LIMIT when the maximum number of states is reached.*)
    val bfs : s0:Big.bg -> priorities:p_class list -> max:int ->
	      iter_f:(int -> Big.bg -> unit) -> graph * stats

    (** Compute the string representation in PRISM [tra] format of a transition
        system. *)
    val to_prism : graph -> string

    (** Compute the string representation in [dot] format of a transition
        system. *)
    val to_dot : graph -> name:string -> string

    (** Compute the string representation in PRISM [lab] format of the labelling
        function of a transition system. *)
    val to_lab : graph -> string
			   
    val iter_states : f:(int -> Big.bg -> unit) -> graph -> unit

    val write_prism : graph -> name:string -> path:string -> int

    val write_lab : graph -> name:string -> path:string -> int

    val write_dot : graph -> name:string -> path:string -> int
  end

(** {6 Simulation traces} *)

module Trace : sig		  
	       
    type limit = float
		   
    exception LIMIT of graph * stats

    exception DEADLOCK of graph * stats * limit

    val sim :
      s0:Big.bg ->
      priorities:p_class list -> init_size:int ->
      stop:limit -> iter_f:(int -> Big.bg -> unit) -> graph * stats

    val to_prism : graph -> string
			  
    val to_dot : graph -> name:string -> string

    val to_lab : graph -> string
			
    val iter_states : f:(int -> Big.bg -> unit) -> graph -> unit

    val write_svg : graph -> name:string -> path:string -> int
							 
    val write_prism : graph -> name:string -> path:string -> int
							   
    val write_lab : graph -> name:string -> path:string -> int

    val write_dot : graph -> name:string -> path:string -> int

  end							  

(**/**)
