(** This module provides a basic interface to the MiniCARD cardinality
    solver.

    @author Michele Sevegnani *)

(** {2 Datatypes} *)

type t
(** The type of MiniCARD solvers. *)

type var
(** The type of variables. *)

type lit
(** The type of literals. *)

(** The type of variable values. *)
type value = False | True | Unknown

(** The type of MiniCARD solver solutions. *)
type solution = SAT | UNSAT

type stat = {
  v : int;  (** Number of variables. *)
  c : int;  (** Number of clauses. *)
  mem : float;  (** Memory used in MB. *)
  cpu : float;  (** CPU time in seconds. *)
}

(** The class implementing MiniCARD solvers. *)
class solver :
  object
    val solver : t
    (** MiniCARD instance.*)

    method add_clause : lit list -> unit
    (** Add a clause (i.e. disjunction of literals) to the set of problem
        constraints. A clause is represented as a list of literals. *)

    method set_verbosity : int -> unit
    (** Set verbosity level (0=silent, 1=some, 2=more). *)

    method add_at_most : lit list -> int -> unit
    (** Add an {i at most} cardinality constraint to the set of problem
        constraints. A clause is represented as a list of literals. *)

    method add_at_least : lit list -> int -> unit
    (** Add an {i at least} cardinality constraint to the set of problem
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
    (** Return the value associated to a variable. {b Note}, this method can
        only be invoked after [solve] returned [SAT].

        @raise Invalid_argument when the input variable is not a valid index. *)

    method get_stats : stat
    (** Return some current statistics. {b Note}, this method can only be
        invoked after [solve] has been invoked.*)

    method print_stats : unit
    (** Print some current statistics to standard output. {b Note}, this
        method can only be invoked after [solve] has been invoked.*)
  end

val string_of_value : value -> string
(** Convert a value to a string. *)

external pos_lit : var -> lit = "ocaml_minicard_pos_lit"
(** Return the positive literal for a variable. *)

external neg_lit : var -> lit = "ocaml_minicard_neg_lit"
(** Return the negative literal for a variable. *)

external negate : lit -> lit = "ocaml_minicard_negate"
(** Negate a literal. *)

(**/**)
