(** Application conditions for bigraph reaction rules.

    @author Blair Archibald *)

(** Output signature of the functor {!AppCond.Make}. *)
module type C = sig
  type loc = Ctx | Param

  type t = { neg : bool; where : loc; pred : Big.t }

  val check_cond : t -> ctx:Big.t -> param:Big.t -> bool
end

(** Functor building an implementation of the application conditions given a
    bigraph matching engine. *)
module Make (S : Solver.M) : C
