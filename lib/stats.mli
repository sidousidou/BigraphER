(** This module defines execution statistics for Reactive Systems.
    @author Michele Sevegnani*)

(** The type of statistics. *)
type t = { time : float;     (** Build time            *)
           states : int;     (** Number of states      *) 
           trans : int;      (** Number of transitions *)
           occs : int;      (** Number of occurrences *)
         }
         
(** Initialise a new value of type {!type:Stats.t}. Argument [t0] is the time
    when execution started. *)
val init : t0:float -> states:int -> trans:int -> occs:int -> t

(** Return a description consisting of list of triples in the form
   [(description, value, run_dependant)]. *)
val descr : t -> (string * string * bool) list

(** Return a string representation of execution statistics. Example:

{[Build time:    0.964225
States:        128
Transitions:   154
Occurrences:   430]} *)
val to_string : t -> string

(** Same as {!val:Stats.to_string} but in JSON format. Example:

{[{
  "stats": {
    "time": 3.198,
    "states": 128,
    "trans": 154,
    "occs": 430
  }
}]} *)
val to_json : t -> string
