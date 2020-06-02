(** This module provides operations on probabilistic BRS.

    @author Michele Sevegnani *)

(** Output signature of the functor {!Pbrs.Make}. *)
module type T = sig
  include TsType.RS with type label = float and type limit = int

  val weight : react -> label
end

(** Functor building a concrete implementation of a probabilistic BRS given a
    matching engine. *)
module Make (S : Solver.M) : T

(**/**)
