(******************************************************************************)
(*                                                                            *)
(*                                  BigraphER                                 *)
(*                                                                            *)
(*                   Michele Sevegnani, University of Glasgow                 *)
(*                                                                            *)
(******************************************************************************)

(** This module provides export operations on bigraphs.
    @author Michele Sevegnani
    @version 0.3.0 *)

(** @raise Failure when [dot] is not in the execution path.*)
val check_graphviz : unit -> unit

(** [write_big b name path v] writes an svg representation of bigraph
    [b] on file [name] in directory [path]. Argument [v] is a flag for
    verbosity.
    @raise Failure when [dot] terminates unexpectedly.*)
val write_big : Big.bg -> string -> string -> bool -> unit 

(** Same as {!Export.write_big} but for transition systems. *)
val write_ts: Brs.ts -> string -> string -> bool -> unit

(** Same as {!Export.write_big} but for Continuous Time Markov Chains. *)
val write_ctmc: Sbrs.ctmc -> string -> string -> bool -> unit

(** Export a transition system to a text file in PRISM format. *)
val write_ts_prism: Brs.ts -> string -> string -> bool -> unit

(** Same as {!Export.write_ts_prism} but for Continuous Time Markov Chains. *)
val write_ctmc_prism: Sbrs.ctmc -> string -> string -> bool -> unit

(** Export a labelling function to a text file in PRISM format. *) 
val write_csl: (int, int) Hashtbl.t -> string -> string -> bool -> unit
