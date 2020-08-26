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

val string_of_value : value -> string
(** String representation of a solver value. *)

type occ = {
  nodes : Iso.t;  (** One-to-one mapping over nodes. *)
  edges : Iso.t;  (** One-to-one mapping over edges. *)
  hyper_edges : Fun.t;  (** Mapping over hyper-edges.*)
}
(** The type of occurrences. *)

val pp_occ : Format.formatter -> occ -> unit
(** Pretty printer *)

(** External solver interface. *)
module type E = sig
  type t

  type var

  type lit

  val create : unit -> t

  val set_verbosity : t -> int -> unit

  val add_clause : t -> lit list -> unit

  val add_at_most : t -> lit list -> int -> unit

  val add_at_least : t -> lit list -> int -> unit

  val add_exactly : t -> lit list -> int -> unit

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
  val solver_type : solver_t

  val string_of_solver_t : string

  include E

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
  (** Raised when the matching pattern has no nodes. *)

  val solver_type : solver_t
  (** The type of the solver .*)

  val string_of_solver_t : string
  (** String representation of a solver type. *)

  val occurs : target:Big.t -> pattern:Big.t -> bool
  (** [occurs ~target ~pattern] returns [true] if the [~pattern] occurs in
      the [~target], [false] otherwise. *)

  val occurrence : target:Big.t -> pattern:Big.t -> occ option
  (** [occurrence ~target ~pattern] returns an occurrence if the [~pattern]
      occurs in the [~target]. Different occurrences might be returned
      depending on which external solver is used.

      @raise NODE_FREE when the [~pattern] has an empty node set. *)

  val auto : Big.t -> (Iso.t * Iso.t) list
  (** Compute the non-trivial automorphisms of a bigraph. The elements of
      each output pair are an automorphism over the place graph and an
      automorphism over the link graph, respectively. *)

  val occurrences : target:Big.t -> pattern:Big.t -> occ list
  (** [occurrences ~target ~pattern] returns a list of occurrences. Each
      occurrence is normalised by picking the smallest occurrence
      (lexicographic ordering) in the symmetry group.

      @raise NODE_FREE when the [~pattern] has an empty node set. *)

  val occurrences_raw : target:Big.t -> pattern:Big.t -> occ list
  (** Same as {!Solver.M.occurrences} but without filtering symmetric
      occurrences out.

      @raise NODE_FREE when the [~pattern] has an empty node set. *)

  val equal : Big.t -> Big.t -> bool
  (** [equal a b] returns [true] if bigraphs [a] and [b] are isomorphic,
      [false] otherwise. *)

  val equal_key : Big.t -> Big.t -> bool
  (** Same as {!Solver.M.equal} but with fewer checks prior to the solver
      invocation. This function is intended to be used after equality over
      keys has already failed. *)

  (** Memoised interface. *)
  module Memo : sig
    val auto : Big.t -> Sparse.t -> (Iso.t * Iso.t) list

    val occurs : target:Big.t -> pattern:Big.t -> Sparse.t -> bool

    val occurrence : target:Big.t -> pattern:Big.t -> Sparse.t -> occ option

    val occurrences :
      target:Big.t ->
      pattern:Big.t ->
      Sparse.t ->
      (Iso.t * Iso.t) list ->
      occ list

    val occurrences_raw :
      target:Big.t -> pattern:Big.t -> Sparse.t -> occ list
  end
end

(** Bigraph matching engine based on solver [S] *)
module Make_SAT (S : S) : M
