(** This module provides operations on stochastic BRS.

    @author Michele Sevegnani *)

(** Output signature of the functor {!Sbrs.Make}. *)
module type T = sig
  include TsType.RS with type label = float and type limit = float
  val rate : react -> float
end

(** Functor building a concrete implementation of a stochastic BRS given a
    matching engine. *)
module Make (S : Solver.M) : T

(**/**)
