(** Types of solvers.

    @author Michele Sevegnani *)

(** Solver type. *)
type solver_t = MSAT  (** MiniSAT *) | MCARD  (** MiniCARD *)

type stats = {
  v : int;  (** Number of variables. *)
  c : int;  (** Number of clauses. *)
  mem : float;  (** Memory used in MB. *)
  cpu : float;  (** CPU time in seconds. *)
}
(** Solver statistics. *)
