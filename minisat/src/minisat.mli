(** This module provides a basic interface to the MiniSAT solver.

    @version 0.5.3
    @author Michele Sevegnani *)

(** {2 Datatypes & exceptions} *)

(** The type of MiniSAT solvers. *)
type s

(** The type of variables. *)
type var = int

(** The type of literals. *)
type lit = int

(** The type of variable values. *)
type value = False | True | Unknown

(** The type of MiniSAT solver solutions. *)
type solution = SAT | UNSAT

(** The type of MiniSAT solver statistics. *)
type stat = {
  v : int;  (** Number of variables. *)
  c : int;  (** Number of clauses. *)
  mem : float;  (** Memory used in MB. *)
  cpu : float;  (** CPU time in seconds. *)
}

(** Raised by {!Minisat.solver.value_of} when using an invalid argument. *)
exception MINISAT of string

(** The class implementing MiniSAT solvers. *)
class solver :
  object
    val solver : s
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

        @raise Minisat.MINISAT when the input variable is not a valid index. *)

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
