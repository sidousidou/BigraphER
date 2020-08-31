(** Application conditions for bigraph reaction rules.

    @author Blair Archibald *)

type t = {
  neg : bool;  (** Negative application condition. *)
  where : loc;
      (** Specify whether the predicate has to be checked against the context
          or the parameter of a match. *)
  pred : Big.t;  (** Predicate. *)
}
(** The type of application conditions. *)

and loc = Ctx  (** Context. *) | Param  (** Parameter. *)

(** Output signature of the functor {!AppCond.Make}. *)
module type C = sig
  val check_cond : t -> ctx:Big.t -> param:Big.t -> bool
  (** Check an application condition given a matching context [ctx] and
      parameter [param]. *)

  val check_cond_memo :
    t ->
    ctx:Big.t ->
    ctx_trans:Sparse.t ->
    param:Big.t ->
    param_trans:Sparse.t ->
    bool
  (** Memoised check *)
end

(** Functor building an implementation of the application conditions given a
    bigraph matching engine. *)
module Make (S : Solver.M) : C
