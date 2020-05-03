(** This module provides a basic interface to the MiniSAT solver.

    @author Michele Sevegnani *)

(** {2 Datatypes} *)

type t
(** The type of MiniSAT solvers. *)

type var
(** The type of variables. *)

type lit
(** The type of literals. *)

(** The type of variable values. *)
type value = False | True | Unknown

(** The type of MiniSAT solver solutions. *)
type solution = SAT | UNSAT

type stat = {
  v : int;  (** Number of variables. *)
  c : int;  (** Number of clauses. *)
  mem : float;  (** Memory used in MB. *)
  cpu : float;  (** CPU time in seconds. *)
}
(** The type of MiniSAT solver statistics. *)

(** The class implementing MiniSAT solvers. *)
class solver :
  object
    val solver : t
    (** MiniSAT instance.*)

    method add_clause : lit list -> unit
    (** Add a clause (i.e. disjunction of literals) to the set of problem
        constraints. A clause is represented as a list of literals. *)

    method new_var : var
    (** Create a fresh variable. *)

    method simplify : unit
    (** [simplify] can be called before [solve] to simply the set of problem
        constrains. It will first propagate all unit information and the
        remove all satisfied constraints. *)

    method solve : solution
    (** Find a solution to the current sat problem. *)

    method value_of : var -> value
    (** Return the value associated to a variable.

        @raise Invalid_argument when the input variable is not a valid index. *)

    method get_stats : stat
    (** Return some current statistics. *)

    method print_stats : unit
    (** Print some current statistics to standard output. *)
  end

val string_of_value : value -> string
(** Convert a value to a string. *)

external pos_lit : var -> lit = "ocaml_minisat_pos_lit"
  [@@noalloc]
(** Return the positive literal for a variable. *)

external neg_lit : var -> lit = "ocaml_minisat_neg_lit"
  [@@noalloc]
(** Return the negative literal for a variable. *)

(**/**)
