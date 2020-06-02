(** This module provides operations on BRS.

    @author Michele Sevegnani *)

(** Output signature of the functor {!Brs.Make}. *)
module type T = TsType.RS with type label = unit and type limit = int

(** Functor building a concrete implementation of a BRS given a matching
    engine. *)
module Make (S : Solver.M) : T
(**/**)
