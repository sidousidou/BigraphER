(** This module provides operations on action BRS.

    @author Paulius Dilkas
    @author Blair Archibald *)

(** Functor building a concrete implementation of an action BRS with Markov
    Decision Process (MDP) semantics given a matching engine. *)
module Make (S : Solver.M) : sig
  include
    TsType.RS with type label = string * int * float and type limit = int

  val action : react -> string
  (** The MDP action this reaction rule belongs to. *)

  val weight : react -> float
  (** The weight of a reaction rule. *)
end

(**/**)
