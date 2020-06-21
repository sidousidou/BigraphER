(** This module provides operations on BRS.

    @author Michele Sevegnani *)

type react = {
  name : string;  (** Name. *)
  rdx : Big.t;  (** Redex, {e i.e.} the left-hand side of a reaction rule. *)
  rct : Big.t;
      (** Reactum, {e i.e.} the right-hand side of a reaction rule. *)
  eta : Fun.t option;  (** Instantiation map. *)
  conds : AppCond.t list;  (** Application conditions. *)
}
(** The type of bigraphical reaction rules.*)

type graph = {
  v : (int * Big.t) Base.H_int.t;
  e : (int * unit * string) Base.H_int.t;
  l : int Base.H_predicate.t;
  preds : Base.S_predicate.t;
}
(** The type of a labelled transition system. *)

(** Output signature of the functor {!Brs.Make}. *)
module type T =
  Rs.RS
    with type react = react
     and type ac := AppCond.t
     and type label = unit
     and type graph = graph
     and type limit = int

(** Functor building a concrete implementation of a BRS given a matching
    engine. *)
module Make (S : Solver.M) : T

(**/**)
