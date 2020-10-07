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

val create : unit -> t
(** Create a MiniSAT instance.*)

val add_clause : t -> lit list -> unit
(** Add a clause (i.e. disjunction of literals) to the set of problem
    constraints. A clause is represented as a list of literals. *)

val set_verbosity : t -> int -> unit
(** Set verbosity level (0=silent, 1=some, 2=more). *)

val new_var : t -> var
(** Create a fresh variable. *)

val simplify : t -> unit
(** [simplify] can be called before [solve] to simply the set of problem
    constrains. It will first propagate all unit information and the remove
    all satisfied constraints. *)

val solve : t -> solution
(** Find a solution to the current sat problem. *)

val value_of : t -> var -> value
(** Return the value associated to a variable. {b Note}, this method can only
    be invoked after [solve] returned [SAT].

    @raise Invalid_argument when the input variable is not a valid index. *)

val get_models : ?vars:var list -> t -> var list list
(** Return a list of valid models. Note, only [true] variables are included
    in each model. Optional argument [vars] is a list of variables that are
    banned at each iteration. If this argument is missing all variables are
    banned. *)

val get_stats : t -> stat
(** Return some current statistics. {b Note}, this method can only be invoked
    after [solve] has been invoked. *)

val print_stats : t -> unit
(** Print some current statistics to standard output. {b Note}, this method
    can only be invoked after [solve] has been invoked. *)

val string_of_value : value -> string
(** Convert a value to a string. *)

external pos_lit : var -> lit = "ocaml_minisat_pos_lit"
(** Return the positive literal for a variable. *)

external neg_lit : var -> lit = "ocaml_minisat_neg_lit"
(** Return the negative literal for a variable. *)

external negate : lit -> lit = "ocaml_minisat_negate"
(** Negate a literal. *)

(**/**)
