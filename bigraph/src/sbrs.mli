(** This module provides operations on stochastic BRS.

    @author Michele Sevegnani *)

(** Functor building a concrete implementation of a stochastic BRS given a
    matching engine. *)
module Make (S : Solver.M) : sig
  include TsType.RS with type label = float and type limit = float

  val rate : react -> float
end

(**/**)
