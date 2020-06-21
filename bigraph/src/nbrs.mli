(** This module provides operations on action BRS.

    @author Paulius Dilkas
    @author Blair Archibald *)

type react = {
  name : string;  (** Reaction rule name. *)
  action : string;  (** Action name. *)
  reward : int;  (** Reward. *)
  rdx : Big.t;  (** Redex, {e i.e.} the left-hand side of a reaction rule. *)
  rct : Big.t;
      (** Reactum, {e i.e.} the right-hand side of a reaction rule. *)
  eta : Fun.t option;  (** Instantiation map. *)
  w : float;  (** Weight. *)
  conds : AppCond.t list;  (** Application conditions. *)
}
(** The type of action bigraphical reaction rules.*)

type graph = {
  v : (int * Big.t) Base.H_int.t;
  e : (int * (string * int * float) * string) Base.H_int.t;
  l : int Base.H_predicate.t;
  preds : Base.S_predicate.t;
}
(** The type of a labelled Markov Decision Process (MDP).*)

(** Output signature of the functor {!Nbrs.Make}. *)
module type T = sig
  include
    Rs.RS
      with type react = react
       and type ac := AppCond.t
       and type label = string * int * float
       and type graph = graph
       and type limit = int

  val action : react -> string
  (** The MDP action this reaction rule belongs to. *)

  val weight : react -> float
  (** The weight of a reaction rule. *)
end

(** Functor building a concrete implementation of an action BRS with Markov
    Decision Process (MDP) semantics given a matching engine. *)
module Make (S : Solver.M) : T

(**/**)
