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

(** Type of solver solutions. *)
type solution = SAT | UNSAT

(** Type of solver values. *)
type value = False | True | Unknown

val string_of_value : solver_t -> string
(** String representation of a solver value. *)

(** External solver interface. *)
module type E = sig
  type t

  type var

  type lit

  val create : unit -> t

  val add_clause : t -> lit list -> unit

  val add_at_most : t -> lit list -> int -> unit

  val add_at_least : t -> lit list -> int -> unit

  val add_at_exactly : t -> lit list -> int -> unit

  val new_var : t -> var

  val simplify : t -> unit

  val solve : t -> solution

  val value_of : t -> var -> value

  val get_stats : t -> stats

  val positive_lit : var -> lit

  val negative_lit : var -> lit

  val negate : lit -> lit
end

(** Solver interface. *)
module type S = sig
  include E

  val solver_type : solver_t

  val string_of_solver_t : string

  val new_var_vector : t -> int -> var array

  val new_var_matrix : t -> int -> int -> var array array

  val add_clauses : t -> lit list list -> unit

  val add_implication : t -> lit -> lit list list -> unit

  val add_iff : t -> lit -> lit list list -> unit

  val add_conj_pairs : t -> (lit * lit) list -> unit

  val add_fun : t -> var array array -> unit

  val add_injection : t -> var array array -> unit

  val add_bijection : t -> var array array -> unit

  val ban : t -> var list -> unit

  val get_iso : t -> var array array -> Iso.t

  val get_fun : t -> var array array -> Fun.t
end

module MS : S
(** Instance of MiniSat solver. *)

module MC : S
(** Instance of MiniCARD solver. *)

(** The type of a bigraph matching engine. *)
module type M = sig
  exception NODE_FREE

  val solver_type : solver_t

  val string_of_solver_t : string

  type t = { nodes : Iso.t; edges : Iso.t; hyper_edges : Fun.t }

  val occurs : target:Big.t -> pattern:Big.t -> bool

  val occurrence : target:Big.t -> pattern:Big.t -> t option

  val occurrence_memo : target:Big.t -> pattern:Big.t -> Sparse.t -> t option

  val occurrences : target:Big.t -> pattern:Big.t -> t list

  val equal : Big.t -> Big.t -> bool

  val equal_key : Big.t -> Big.t -> bool
end

(** Bigraph mathing engine based on solver [S] *)
module Match (S : S) : M
