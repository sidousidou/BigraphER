(** This module provides operations on BRS.

    @author Michele Sevegnani *)

(** Functor building a concrete implementation of a BRS given a matching
    engine. *)
module Make (S : Solver.M) :
  TsType.RS with type label = unit and type limit = int

(**/**)
