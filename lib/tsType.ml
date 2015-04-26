module type T = sig

    type t
    type stats
    type limit
    type p_class
	   
    (** Raised when the limit is reached. *)
    exception LIMIT of t * stats
	   
    (** [bfs s p max f] computes the transition system of the BRS specified by
        initial state [s] and priority classes [p]. Argument [max] is the
        maximum number of states of the transition system and [f] is a function
        that is applied at every loop. Priority classes are assumed to be sorted
        by priority, {e i.e.} the first element in the list is the class with
        the highest priority.
        @raise LIMIT when the maximum number of states is reached. *)
    val bfs : Big.bg -> p_class list -> int -> (int -> Big.bg -> unit) ->
	      t * stats

    (** Similar to {!Brs.bfs} but only one simulation path is computed. In this
        case, parameter [max] indicates a treshold for terminating the
        computation. For example, the maximum simulation time in a Continuous
        Time Markov Chain. 
        @raise LIMIT when limit [max] is reached. *)
    val sim : Big.bg -> p_class list -> limit -> (int -> Big.bg -> unit) ->
	      t * stats

    (** Textual representation of a transition system. The format is compatible
        with [tra] PRISM format. *)
    val to_prism : t -> string

    (** Export the labelling function of a transition system to a text file in
        [lab] PRISM format. *)
    val to_lab : t -> string

    (** Compute the string representation in [dot] format of a transition
        system. *)
    val to_dot : t -> string

    (** [iter_states f t] applies function [f] to every state of [t]. The first
        argument of [f] is the state index while the second is the
        corresponding bigraph. *)
    val iter_states : (int -> Big.bg -> unit) -> t -> unit

  end
