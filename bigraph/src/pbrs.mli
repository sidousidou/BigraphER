(** This module provides operations on probabilistic BRS.

    @author Michele Sevegnani *)

type react = {
  name : string;  (** Name. *)
  rdx : Big.t;  (** Redex, {e i.e.} the left-hand side of a reaction rule. *)
  rct : Big.t;
      (** Reactum, {e i.e.} the right-hand side of a reaction rule. *)
  eta : Fun.t option;  (** Instantiation map. *)
  w : float;  (** Weight. *)
  conds : AppCond.t list;  (** Application conditions. *)
}
(** The type of probabilistic bigraphical reaction rules.*)

type graph = {
  v : (int * Big.t) Base.H_int.t;
  e : (int * float * string) Base.H_int.t;
  l : int Base.H_predicate.t;
  preds : Base.S_predicate.t;
}
(** The type of a labelled Discrete Time Markov Chain (DTMC).*)

(** Output signature of the functor {!Pbrs.Make}. *)
module type T = sig
  include
    Rs.RS
      with type react = react
       and type ac := AppCond.t
       and type label = float
       and type graph = graph
       and type limit = int

  val weight : react -> label
end

(** Functor building a concrete implementation of a probabilistic BRS given a
    matching engine. *)
module Make (S : Solver.M) : T

(**/**)
