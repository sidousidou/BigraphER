(** This module defines execution statistics for Reactive Systems.

    @author Michele Sevegnani*)

type t = {
  time : float;  (** Build time *)
  states : int;  (** Number of states *)
  trans : int;  (** Number of transitions *)
  occs : int;  (** Number of occurrences *)
}
(** The type of statistics. *)

val init : t0:float -> states:int -> trans:int -> occs:int -> t
(** Initialise a new value of type {!type:Stats.t}. Argument [t0] is the time
    when execution started. *)

val descr : t -> (string * string * bool) list
(** Return a description consisting of list of triples in the form
    [(description, value, run_dependant)]. *)

val to_string : t -> string
(** Return a string representation of execution statistics. Example:

    {[
      Build time:    0.964225
      States:        128
      Transitions:   154
      Occurrences:   430
    ]} *)
