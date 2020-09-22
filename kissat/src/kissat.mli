(** This module provides a basic interface to the Kissat SAT solver.

    @author Michele Sevegnani *)

(** {2 Datatypes} *)

type t
(** The type of Kissat solvers. *)

type var
(** The type of variables. *)

type lit
(** The type of literals. *)

(** The type of variable values. *)
type value = False | True | Unknown

(** The type of Kissat solver solutions. *)
type solution = SAT | UNSAT

type stat = {
  v : int;  (** Number of variables. *)
  c : int;  (** Number of clauses. *)
}

val create : unit -> t

val solve : t -> (solution, int) result

val value_of : t -> var -> value

val new_var : t -> var

val add_clause : t -> lit list -> unit

val string_of_value : value -> string
(** Convert a value to a string. *)

val pos_lit : var -> lit
(** Return the positive literal for a variable. *)

val neg_lit : var -> lit
(** Return the negative literal for a variable. *)

val negate : lit -> lit
(** Negate a literal. *)

val print_stats : t -> unit

val set_option : t -> string -> int -> int

val get_option : t -> string -> int

val get_stats : t -> stat

(**/**)
