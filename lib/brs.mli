(** This module provides operations on BRS.
    @author Michele Sevegnani *)

(** The type of bigraphical reaction rules.*)
type react =
  { rdx : Big.bg;                  (* Redex   --- lhs   *)
    rct : Big.bg;                  (* Reactum --- rhs   *)
    eta : int Fun.t option         (* Instantiation map *)
  }
       
(** The type of priority classes as lists of reaction rules. *)
type p_class =
  | P_class of react list  (** Priority class *)
  | P_rclass of react list (** Reducible priority class *)

(** String representation of a reaction. *)
val to_string_react : react -> string

(** Return [true] if the inner (outer) interfaces of the redex (reactum) are
    equal and if the redex is solid. [false] otherwise. *)
val is_valid_react : react -> bool

(** Return [true] if all the reaction rules in the priority class are valid,
    [false] otherwise. *)
val is_valid_priority : p_class -> bool

(** Return [true] if a list of priority classes contains at least a non reducing
    priority class, [false] otherwise. *)
val is_valid_priority_list : p_class list -> bool

(** Compute all the possible evolutions in one step. Total number of occurrences
    also returned. *)
val step : Big.bg -> react list -> Big.bg list * int

(** Reduce a reducible class to the fixed point. Return the input state if no
    rewriting is performed. The fixed point and the number of rewriting steps
    performed are returned otherwise. *)   
val fix : Big.bg -> react list -> Big.bg * int

(** Scan priority classes and reduce a state. Stop when no more rules can be
    applied or when a non reducing priority class is enabled. The output integer
    is the number of rewriting steps performed in the loop. *)
val rewrite : Big.bg -> int -> p_class list -> Big.bg * int

(** {6 Transition systems} *)
							  
(** The type of transition systems. *)
type ts_g = {
    v : (Big.bg_key, (int * Big.bg)) Hashtbl.t; (** States *)
    e : (int, int) Hashtbl.t;                   (** Transition relation *)
    l : (int, int) Hashtbl.t;                   (** Labelling function *) 
  }

type ts_stats = {
    time : float;  (** Execution time *)
    states : int;    (** Number of states *)
    trans : int;    (** Number of reaction *)
    occs : int;    (** Number of occurrences *)
  }

module TransitionSystem : sig		  
    
    (** Raised when the size of the transition system reaches the limit. *)
    exception MAX of ts_g * ts_stats
			      
    (** [bfs s0 priorities max f] computes the transition system of the BRS
        specified by initial state [s] and priority classes [p]. [l] is the
        maximum number of states of the transition system. [n] is the
        initialisation size for the edges and [f] is a function that is applied
        at every loop. Priority classes are assumed to be sorted by priority,
        i.e. the first element in the list is the class with the highest
        priority.  

        @raise Brs.LIMIT when the maximum number of states is reached. *)
    val bfs : s0:Big.bg -> priorities:p_class list -> max:int ->
	      iter_f:(int -> Big.bg -> unit) -> ts_g * ts_stats

    (** Compute the string representation in PRISM [tra] format of a transition
        system. *)
    val to_prism : ts_g -> string

    (** Compute the string representation in [dot] format of a transition
        system. *)
    val to_dot : ts_g -> name:string -> string

    (** Compute the string representation in PRISM [lab] format of the labelling
        function of a transition system. *)
    val to_lab : ts_g -> string
			   
    val iter_states : f:(int -> Big.bg -> unit) -> ts_g -> unit

    val write_prism : ts_g -> name:string -> path:string -> int

    val write_lab : ts_g -> name:string -> path:string -> int

    val write_dot : ts_g -> name:string -> path:string -> int
  end
			    
(** {6 Simulation traces} *)
							  
(* (\** The type of simulation trace. *\) *)
(* type trace = { *)
(*     v : (Big.bg_key, (int * Big.bg)) Hashtbl.t; (\** States *\) *)
(*     e : (int, int) Hashtbl.t;                   (\** Transition relation *\) *)
(*     l : (int, int) Hashtbl.t;                   (\** Labelling function *\)  *)
(*   } *)

(* type sim_stats = { *)
(*     time : float;  (\** Execution time *\) *)
(*     states : int;    (\** Number of states *\) *)
(*     trans : int;    (\** Number of reaction *\) *)
(*     occs : int;    (\** Number of occurrences *\) *)
(*   } *)

(* exception SIM_LIMIT of trace * sim_stats *)
							
(* (\** Compute a random reaction. *)
(*     @raise NODE_FREE when [p] has an empty node set. *\) *)
(* val random_step : Big.bg -> react list -> Big.bg option *)

(* (\** Similar to {!Brs.bfs} but only one simulation path is computed. In this *)
(*     case, parameter [l] indicates the maximum number of simulation steps. *\) *)
(* val sim : s0:Big.bg -> priorities:p_class list -> max_steps:int -> *)
(* 	  iter_f:(int -> Big.bg -> unit) -> trace * sim_stats *)
						     
(**/**)
